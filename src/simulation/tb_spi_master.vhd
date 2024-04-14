library ieee;
use ieee.math_real.all;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

library vunit_lib;
context vunit_lib.vunit_context;

library osvvm;
use osvvm.TbUtilPkg.all;
use osvvm.RandomPkg.all;

library work;
use work.spi_package.all;

entity tb_spi_master is
  generic (
    runner_cfg                                : string;
    G_RNG_SEED                                : integer    := 2;
    G_SCLK_IDLE_STATE                         : std_ulogic := '1';
    G_SCS_IDLE_STATE                          : std_ulogic := '1';
    G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE : std_ulogic := '1';
    G_MAX_N_CLKS_SCS_TO_SCLK                  : positive   := 3;
    G_MAX_N_CLKS_SCLK_TO_SCS                  : positive   := 5;
    G_MAX_N_BITS                              : positive   := 4;
    G_MAX_SCLK_DIVIDE_HALF                    : positive   := 2;
    G_MAX_N_CLKS_SCLK_TO_LE                   : positive   := 2;
    G_MAX_N_CLKS_LE_WIDTH                     : positive   := 3;
    G_MAX_N_CLKS_RX_SAMPLE_STROBES_DELAY      : natural    := 0
    );
end entity;

architecture arch of tb_spi_master is

  constant C_CLK_PERIOD : time := 10 ns;
  constant C_CONFIG : t_config := (
    max_n_clks_scs_to_sclk             => G_MAX_N_CLKS_SCS_TO_SCLK,
    max_n_clks_sclk_to_scs             => G_MAX_N_CLKS_SCLK_TO_SCS,
    max_n_bits                         => G_MAX_N_BITS,
    max_sclk_divide_half               => G_MAX_SCLK_DIVIDE_HALF,
    max_n_clks_sclk_to_le              => G_MAX_N_CLKS_SCLK_TO_LE,
    max_n_clks_le_width                => G_MAX_N_CLKS_LE_WIDTH,
    max_n_clks_rx_sample_strobes_delay => G_MAX_N_CLKS_RX_SAMPLE_STROBES_DELAY
    );

  procedure wait_until_sclk_edge_away_from_idle(signal sclk : std_ulogic; signal sclk_idle_state : std_ulogic) is
  begin
    if sclk_idle_state = '0' then
      wait until rising_edge(sclk);
    else
      wait until falling_edge(sclk);
    end if;
  end procedure;

  procedure wait_until_sclk_edge_toward_idle(signal sclk : std_ulogic; signal sclk_idle_state : std_ulogic) is
  begin
    if sclk_idle_state = '0' then
      wait until falling_edge(sclk);
    else
      wait until rising_edge(sclk);
    end if;
  end procedure;

  signal clk : std_ulogic := '0';

  signal d_from_peripheral          : std_ulogic_vector(C_CONFIG.max_n_bits - 1 downto 0) := (others => '0');
  signal d_from_peripheral_expected : std_ulogic_vector(C_CONFIG.max_n_bits - 1 downto 0) := (others => '0');
  signal d_to_peripheral            : std_ulogic_vector(C_CONFIG.max_n_bits - 1 downto 0) := (others => '0');

  signal start              : std_ulogic := '0';
  signal sd_from_peripheral : std_ulogic := '0';
  signal sd_to_peripheral   : std_ulogic := '0';

  signal ready           : std_ulogic := '0';
  signal ready_reference : std_ulogic := '0';
  signal sclk            : std_ulogic := '0';
  signal sclk_reference  : std_ulogic := '0';
  signal scs             : std_ulogic := '0';
  signal scs_reference   : std_ulogic := '0';
  signal le              : std_ulogic := '0';
  signal le_reference    : std_ulogic := '0';

  signal sclk_divide_half               : natural range 1 to C_CONFIG.max_sclk_divide_half               := 1;
  signal n_bits                         : natural range 1 to C_CONFIG.max_n_bits                         := 1;
  signal n_clks_scs_to_sclk             : natural range 1 to C_CONFIG.max_n_clks_scs_to_sclk             := 1;
  signal n_clks_sclk_to_scs             : natural range 1 to C_CONFIG.max_n_clks_sclk_to_scs             := 1;
  signal n_clks_sclk_to_le              : natural range 1 to C_CONFIG.max_n_clks_sclk_to_le              := 1;
  signal n_clks_le_width                : natural range 1 to C_CONFIG.max_n_clks_le_width                := 1;
  signal n_clks_rx_sample_strobes_delay : natural range 0 to C_CONFIG.max_n_clks_rx_sample_strobes_delay := 0;

  signal sclk_idle_state                         : std_ulogic := G_SCLK_IDLE_STATE;
  signal scs_idle_state                          : std_ulogic := G_SCS_IDLE_STATE;
  signal transmit_on_sclk_edge_toward_idle_state : std_ulogic := G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE;

  signal transmit_from_peripheral : event_t := new_event("transmit_from_peripheral");

begin

  e_dut : entity work.spi_master(arch)
    generic map(
      G_CONFIG => C_CONFIG
      )
    port map(
      i_clk                                     => clk,
      i_start                                   => start,
      o_ready                                   => ready,
      o_d_from_peripheral                       => d_from_peripheral,
      i_d_to_peripheral                         => d_to_peripheral,
      --
      i_scs_idle_state                          => scs_idle_state,
      i_sclk_idle_state                         => sclk_idle_state,
      i_transmit_on_sclk_edge_toward_idle_state => transmit_on_sclk_edge_toward_idle_state,
      --
      i_sclk_divide_half_minus_1                => to_unsigned(sclk_divide_half - 1, ceil_log2(C_CONFIG.max_sclk_divide_half)),
      i_n_bits_minus_1                          => to_unsigned(n_bits - 1, ceil_log2(C_CONFIG.max_n_bits)),
      i_n_clks_scs_to_sclk_minus_1              => to_unsigned(n_clks_scs_to_sclk - 1, ceil_log2(C_CONFIG.max_n_clks_scs_to_sclk)),
      i_n_clks_sclk_to_scs_minus_1              => to_unsigned(n_clks_sclk_to_scs - 1, ceil_log2(C_CONFIG.max_n_clks_sclk_to_scs)),
      i_n_clks_sclk_to_le_minus_1               => to_unsigned(n_clks_sclk_to_le - 1, ceil_log2(C_CONFIG.max_n_clks_sclk_to_le)),
      i_n_clks_le_width_minus_1                 => to_unsigned(n_clks_le_width - 1, ceil_log2(C_CONFIG.max_n_clks_le_width)),
      i_n_clks_rx_sample_strobes_delay          => to_unsigned(n_clks_rx_sample_strobes_delay, ceil_log2(C_CONFIG.max_n_clks_rx_sample_strobes_delay + 1)),
      --
      o_le                                      => le,
      o_scs                                     => scs,
      o_sclk                                    => sclk,
      o_sd_to_peripheral                        => sd_to_peripheral,
      i_sd_from_peripheral                      => sd_from_peripheral
      );


  CreateClock(clk, C_CLK_PERIOD);

  main : process
    variable RV : RandomPType;
  begin
    test_runner_setup(runner, runner_cfg);

    RV.InitSeed(G_RNG_SEED);
    info("G_RNG_SEED = " & to_string(G_RNG_SEED));

    --
    -- by default, use the testbench generics
    --
    scs_idle_state                          <= G_SCS_IDLE_STATE;
    sclk_idle_state                         <= G_SCLK_IDLE_STATE;
    transmit_on_sclk_edge_toward_idle_state <= G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE;
    sclk_divide_half                        <= C_CONFIG.max_sclk_divide_half;
    n_bits                                  <= C_CONFIG.max_n_bits;
    n_clks_scs_to_sclk                      <= C_CONFIG.max_n_clks_scs_to_sclk;
    n_clks_sclk_to_scs                      <= C_CONFIG.max_n_clks_sclk_to_scs;
    n_clks_sclk_to_le                       <= C_CONFIG.max_n_clks_sclk_to_le;
    n_clks_le_width                         <= C_CONFIG.max_n_clks_le_width;
    n_clks_rx_sample_strobes_delay          <= C_CONFIG.max_n_clks_rx_sample_strobes_delay;
    --
    WaitForClock(clk, 1);
    --
    info("-- begin init");
    info("scs_idle_state = " & to_string(scs_idle_state));
    info("sclk_idle_state = " & to_string(sclk_idle_state));
    info("transmit_on_sclk_edge_toward_idle_state = " & to_string(transmit_on_sclk_edge_toward_idle_state));
    info("sclk_divide_half = " & to_string(sclk_divide_half));
    info("n_bits = " & to_string(n_bits));
    info("n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));
    info("n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
    info("n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
    info("n_clks_le_width = " & to_string(n_clks_le_width));
    info("n_clks_rx_sample_strobes_delay = " & to_string(n_clks_rx_sample_strobes_delay));
    info("-- end init");

    WaitForClock(clk, 10);

    while test_suite loop
      if run("01_all_sclk_scs_idle_cases") then
        start <= '1';
        WaitForClock(clk, 1);
        start <= '0';

        WaitForClock(clk, 1);
        check_equal(ready, '0', result("for ready"));
        wait until rising_edge(ready);
        WaitForClock(clk, 2);
        check_equal(ready, '0', result("for ready"));
      elsif run("02_all_sclk_transmit_edge_cases") then
        start <= '1';
        WaitForClock(clk, 1);
        start <= '0';

        WaitForClock(clk, 1);
        check_equal(ready, '0', result("for ready"));

        wait until ready = '1';
      elsif run("03_sclk_divide") then
        for divide_half in 1 to C_CONFIG.max_sclk_divide_half loop
          sclk_divide_half <= divide_half;
          WaitForClock(clk, 1);
          info("sclk_divide_half = " & to_string(sclk_divide_half));

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          WaitForClock(clk, 1);
          check_equal(ready, '0', result("for ready"));

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      elsif run("04_n_bits") then
        for bits in 1 to C_CONFIG.max_n_bits loop
          n_bits <= bits;
          WaitForClock(clk, 1);
          info("n_bits = " & to_string(n_bits));

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          WaitForClock(clk, 1);
          check_equal(ready, '0', result("for ready"));

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      elsif run("05_transmit") then
        for k in 1 to 20 loop
          d_to_peripheral <= RV.RandSlv(d_to_peripheral'length);
          WaitForClock(clk, 1);

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      elsif run("06_receive") then
        info("deterministic");
        for k in 1 to 20 loop
          d_from_peripheral_expected <= (others => '1');
          WaitForClock(clk, 1);

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;

        info("random");
        for k in 1 to 20 loop
          d_from_peripheral_expected <= RV.RandSlv(d_from_peripheral_expected'length);
          WaitForClock(clk, 1);

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      elsif run("07_max_n_clks_scs_to_sclk") then
        for clks_scs_to_sclk in 1 to C_CONFIG.max_n_clks_scs_to_sclk loop
          n_clks_scs_to_sclk <= clks_scs_to_sclk;
          WaitForClock(clk, 1);
          info("n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      elsif run("08_max_n_clks_sclk_to_scs") then
        for clks_sclk_to_scs in 1 to C_CONFIG.max_n_clks_sclk_to_scs loop
          n_clks_sclk_to_scs <= clks_sclk_to_scs;
          WaitForClock(clk, 1);
          info("n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      elsif run("09_max_n_clks_rx_sample_strobes_delay") then
        info("deterministic");
        for clks_rx_sample_strobes_delay in 0 to C_CONFIG.max_n_clks_rx_sample_strobes_delay loop
          d_from_peripheral_expected <= (others => '1');
          WaitForClock(clk, 1);

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;

        info("random");
        for clks_rx_sample_strobes_delay in 0 to C_CONFIG.max_n_clks_rx_sample_strobes_delay loop
          d_from_peripheral_expected <= RV.RandSlv(d_from_peripheral_expected'length);
          WaitForClock(clk, 1);

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);
        end loop;
      end if;
    end loop;

    WaitForClock(clk, 10);

    test_runner_cleanup(runner);
    wait;
  end process;

  test_runner_watchdog(runner, 20 us);

  ---------------------------------------------------------------------------
  -- check ready
  ---------------------------------------------------------------------------

  p_generate_ready_reference : process
  begin
    wait until rising_edge(start);
    ready_reference <= '0';

    WaitForClock(clk, 1 + n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half);

    -- either SCS inactive or LE takes longer
    if n_clks_sclk_to_scs > n_clks_sclk_to_le + n_clks_le_width then
      WaitForClock(clk, n_clks_sclk_to_scs);
    else
      WaitForClock(clk, n_clks_sclk_to_le + n_clks_le_width);
    end if;
    ready_reference <= '1';

    WaitForClock(clk, 1);
    ready_reference <= '0';
  end process;

  p_check_ready : process
  begin
    wait on ready, ready_reference;
    wait for 0 fs;
    check_equal(ready, ready_reference, "ready is not equal to ready_reference.");
  end process;

  ---------------------------------------------------------------------------
  -- check SCLK
  ---------------------------------------------------------------------------

  p_generate_sclk_reference : process
  begin
    wait until rising_edge(start);
    sclk_reference <= sclk_idle_state;

    wait until falling_edge(start);
    WaitForClock(clk, n_clks_scs_to_sclk);
    sclk_reference <= not sclk_reference;

    for k in 1 to 2 * n_bits - 1 loop
      WaitForClock(clk, sclk_divide_half);
      sclk_reference <= not sclk_reference;
    end loop;
  end process;

  p_check_sclk : process
  begin
    wait until falling_edge(start);

    while not rising_edge(ready) loop
      wait on sclk, sclk_reference;
      wait for 0 fs;
      check_equal(sclk, sclk_reference, "sclk is not equal to sclk_reference.");
    end loop;
  end process;

  ---------------------------------------------------------------------------
  -- check SCS
  ---------------------------------------------------------------------------

  p_generate_scs_reference : process
  begin
    wait until rising_edge(start);
    scs_reference <= scs_idle_state;

    wait until falling_edge(start);
    scs_reference <= not scs_idle_state;

    WaitForClock(clk, n_clks_sclk_to_scs + (2 * n_bits - 1) * sclk_divide_half + n_clks_scs_to_sclk);
    scs_reference <= scs_idle_state;
  end process;

  p_check_scs : process
  begin
    wait until start = '1';

    while not ready = '1' loop
      wait on scs, scs_reference;
      wait for 0 fs;
      check_equal(scs, scs_reference, "scs is not equal to scs_reference.");
    end loop;
  end process;

  ---------------------------------------------------------------------------
  -- check data
  ---------------------------------------------------------------------------

  p_check_data_to_peripheral : process
  begin
    wait until start = '1';

    for n in 0 to n_bits - 1 loop
      if transmit_on_sclk_edge_toward_idle_state = '1' then
        wait_until_sclk_edge_toward_idle(sclk, sclk_idle_state);
      else
        wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
      end if;
      check_equal(sd_to_peripheral, d_to_peripheral(n), result("for sd_to_peripheral"));
    end loop;
  end process;

  p_generate_sd_data_from_peripheral : process
  begin
    wait until start = '1';

    sd_from_peripheral <= '0';

    for n in n_bits - 1 downto 0 loop
      if transmit_on_sclk_edge_toward_idle_state = '1' then  -- receive on sclk away from idle state
        wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
      else
        wait_until_sclk_edge_toward_idle(sclk, sclk_idle_state);
      end if;

      WaitForClock(clk, n_clks_rx_sample_strobes_delay);
      sd_from_peripheral <= d_from_peripheral_expected(n);

      WaitForClock(clk, 1);
      sd_from_peripheral <= '0';
    end loop;
  end process;

  p_check_data_from_peripheral : process
  begin
    wait until rising_edge(ready);
    check_equal(d_from_peripheral, d_from_peripheral_expected, result("for d_from_peripheral"));
  end process;

end architecture;
