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
    G_MAX_N_CLKS_SCLK_TO_SCS                  : positive   := 4;
    G_MAX_N_BITS                              : positive   := 4;
    G_MAX_SCLK_DIVIDE_HALF                    : positive   := 2
    );
end entity;

architecture arch of tb_spi_master is

  procedure wait_until_scs_active(signal scs : std_ulogic; signal scs_idle_state : std_ulogic) is
  begin
    if scs_idle_state = '0' then
      wait until rising_edge(scs);
    else
      wait until falling_edge(scs);
    end if;
  end procedure;

  procedure wait_until_scs_inactive(signal scs : std_ulogic; signal scs_idle_state : std_ulogic) is
  begin
    if scs_idle_state = '0' then
      wait until falling_edge(scs);
    else
      wait until rising_edge(scs);
    end if;
  end procedure;

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

  constant C_CLK_PERIOD : time := 10 ns;

  signal clk                        : std_ulogic                                   := '0';
  signal d_from_peripheral          : std_ulogic_vector(G_MAX_N_BITS - 1 downto 0) := (others => '0');
  signal d_from_peripheral_expected : std_ulogic_vector(G_MAX_N_BITS - 1 downto 0) := (others => '0');
  signal d_to_peripheral            : std_ulogic_vector(G_MAX_N_BITS - 1 downto 0) := (others => '0');
  signal start                      : std_ulogic                                   := '0';
  signal ready                      : std_ulogic                                   := '0';
  signal sclk                       : std_ulogic                                   := '0';
  signal sd_from_peripheral         : std_ulogic                                   := '0';
  signal sd_to_peripheral           : std_ulogic                                   := '0';
  signal scs                        : std_ulogic                                   := '0';

  signal n_bits         : natural range 1 to G_MAX_N_BITS                := 1;
  signal n_bits_minus_1 : unsigned(ceil_log2(G_MAX_N_BITS) - 1 downto 0) := (others => '1');

  signal sclk_divide_half         : natural range 1 to G_MAX_SCLK_DIVIDE_HALF                := 1;
  signal sclk_divide_half_minus_1 : unsigned(ceil_log2(G_MAX_SCLK_DIVIDE_HALF) - 1 downto 0) := (others => '1');

  signal n_clks_scs_to_sclk         : natural range 1 to G_MAX_N_CLKS_SCS_TO_SCLK                := 1;
  signal n_clks_scs_to_sclk_minus_1 : unsigned(ceil_log2(G_MAX_N_CLKS_SCS_TO_SCLK) - 1 downto 0) := (others => '1');

  signal n_clks_sclk_to_scs         : natural range 1 to G_MAX_N_CLKS_SCLK_TO_SCS                := 1;
  signal n_clks_sclk_to_scs_minus_1 : unsigned(ceil_log2(G_MAX_N_CLKS_SCLK_TO_SCS) - 1 downto 0) := (others => '1');

  signal sclk_idle_state                         : std_ulogic := G_SCLK_IDLE_STATE;
  signal scs_idle_state                          : std_ulogic := G_SCS_IDLE_STATE;
  signal transmit_on_sclk_edge_toward_idle_state : std_ulogic := G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE;

begin

  sclk_divide_half_minus_1   <= to_unsigned(sclk_divide_half - 1, sclk_divide_half_minus_1'length);
  n_bits_minus_1             <= to_unsigned(n_bits - 1, n_bits_minus_1'length);
  n_clks_scs_to_sclk_minus_1 <= to_unsigned(n_clks_scs_to_sclk - 1, n_clks_scs_to_sclk_minus_1'length);
  n_clks_sclk_to_scs_minus_1 <= to_unsigned(n_clks_sclk_to_scs - 1, n_clks_sclk_to_scs_minus_1'length);

  e_dut : entity work.spi_master(arch)
    generic map(
      G_MAX_N_CLKS_SCS_TO_SCLK => G_MAX_N_CLKS_SCS_TO_SCLK,
      G_MAX_N_CLKS_SCLK_TO_SCS => G_MAX_N_CLKS_SCLK_TO_SCS,
      G_MAX_N_BITS             => G_MAX_N_BITS,
      G_MAX_SCLK_DIVIDE_HALF   => G_MAX_SCLK_DIVIDE_HALF
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
      i_sclk_divide_half_minus_1                => sclk_divide_half_minus_1,
      i_n_bits_minus_1                          => n_bits_minus_1,
      i_n_clks_scs_to_sclk_minus_1              => n_clks_scs_to_sclk_minus_1,
      i_n_clks_sclk_to_scs_minus_1              => n_clks_sclk_to_scs_minus_1,
      --
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
    sclk_divide_half                        <= G_MAX_SCLK_DIVIDE_HALF;
    n_bits                                  <= G_MAX_N_BITS;
    n_clks_scs_to_sclk                      <= G_MAX_N_CLKS_SCS_TO_SCLK;
    n_clks_sclk_to_scs                      <= G_MAX_N_CLKS_SCLK_TO_SCS;
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
        for divide_half in 1 to G_MAX_SCLK_DIVIDE_HALF loop
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
        for bits in 1 to G_MAX_N_BITS loop
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
        for clks_scs_to_sclk in 1 to G_MAX_N_CLKS_SCS_TO_SCLK loop
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
        for clks_sclk_to_scs in 1 to G_MAX_N_CLKS_SCLK_TO_SCS loop
          n_clks_sclk_to_scs <= clks_sclk_to_scs;
          WaitForClock(clk, 1);
          info("n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));

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

  test_runner_watchdog(runner, 10 us);

  ---------------------------------------------------------------------------
  -- check SCLK and SCS timing
  ---------------------------------------------------------------------------

  p_check_time_scs_active : process
    variable tic : time := 0 ns;
    variable toc : time := 0 ns;
  begin
    wait until start = '1';

    wait_until_scs_active(scs, scs_idle_state);
    tic := now;

    wait_until_scs_inactive(scs, scs_idle_state);
    toc := now;

    check_equal((toc - tic) / C_CLK_PERIOD, n_clks_scs_to_sclk + n_bits * 2 * sclk_divide_half - sclk_divide_half + n_clks_sclk_to_scs, "The SCS active time is not correct");
  end process;

  p_check_time_scs_to_sclk : process
    variable tic : time := 0 ns;
    variable toc : time := 0 ns;
  begin
    wait until start = '1';

    wait_until_scs_active(scs, scs_idle_state);
    tic := now;

    wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
    toc := now;

    check_equal((toc - tic) / C_CLK_PERIOD, n_clks_scs_to_sclk, "The time difference between SCS active and the first SCLK edge is not correct.");
  end process;

  p_check_time_sclk_to_scs : process
    variable tic : time := 0 ns;
    variable toc : time := 0 ns;
  begin
    wait until start = '1';

    wait_until_scs_active(scs, scs_idle_state);

    -- wait for the last SCLK edge before SCS inactive
    while not scs = scs_idle_state loop
      wait_until_sclk_edge_toward_idle(sclk, sclk_idle_state);
      tic := now;
    end loop;
    toc := now;

    check_equal((toc - tic) / C_CLK_PERIOD, n_clks_sclk_to_scs, "The time difference between the last SCLK edge and SCS inactive is not correct.");
  end process;

  p_check_time_sclk : process
    variable tic            : time    := 0 ns;
    variable toc            : time    := 0 ns;
    variable tic_first_edge : time    := 0 ns;
    variable edge_counter   : integer := 0;
  begin
    wait until start = '1';

    wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
    edge_counter   := 1;
    tic_first_edge := now;
    tic            := now;

    while not scs = scs_idle_state loop
      wait on sclk, scs;
      if not scs = scs_idle_state then
        edge_counter := edge_counter + 1;
        toc          := now;

        check_equal((toc - tic) / C_CLK_PERIOD, sclk_divide_half, "The time difference between two consecutive SCLK edges is not correct (edge_counter = " & to_string(edge_counter) & ").");

        tic := now;
      end if;
    end loop;

    check_equal((toc - tic_first_edge) / C_CLK_PERIOD, n_bits * 2 * sclk_divide_half - sclk_divide_half, "The time difference between the first and the last SCLK edges is not correct.");
  end process;

  ---------------------------------------------------------------------------
  -- check SCLK and SCS idle state
  ---------------------------------------------------------------------------

  p_check_sclk_idle_state : process
  begin
    wait until start = '1';
    check_equal(sclk, sclk_idle_state, result("for sclk"));

    wait until sclk = not sclk_idle_state;

    wait until ready = '1';
    check_equal(sclk, sclk_idle_state, result("for sclk"));
  end process;

  p_check_scs_idle_state : process
  begin
    wait until start = '1';
    check_equal(scs, scs_idle_state, result("for scs"));

    wait until scs = not scs_idle_state;

    wait until ready = '1';
    check_equal(scs, scs_idle_state, result("for scs"));
  end process;

  ---------------------------------------------------------------------------
  -- check sample strobes
  ---------------------------------------------------------------------------

  p_check_sample_strobe_sdi_happens : process
    alias sample_sdi is << signal e_dut.sample_sdi : std_ulogic >>;
  begin
    wait until start = '1';

    if transmit_on_sclk_edge_toward_idle_state then
      wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
    else
      wait_until_sclk_edge_toward_idle(sclk, sclk_idle_state);
    end if;

    check_equal(sample_sdi, '1', result("for sample_sdi"));
    WaitForClock(clk, 1);
    check_equal(sample_sdi, '1', result("for sample_sdi"));
    WaitForClock(clk, 1);
    check_equal(sample_sdi, '0', result("for sample_sdi"));
  end process;

  p_check_sample_strobe_sdo_happens : process
    alias sample_sdo is << signal e_dut.sample_sdo : std_ulogic >>;
  begin
    wait until start = '1';

    if transmit_on_sclk_edge_toward_idle_state then
      wait_until_sclk_edge_toward_idle(sclk, sclk_idle_state);
    else
      wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
    end if;

    check_equal(sample_sdo, '1', result("for sample_sdo"));
    WaitForClock(clk, 1);
    check_equal(sample_sdo, '1', result("for sample_sdo"));
    WaitForClock(clk, 1);
    check_equal(sample_sdo, '0', result("for sample_sdo"));
  end process;

  p_check_sample_strobe_sdi_count : process
    alias sample_sdi is << signal e_dut.sample_sdi : std_ulogic >>;
    variable count                                 : integer := 0;
  begin
    wait until start = '1';

    count := 0;
    while not ready = '1' loop
      wait until rising_edge(sample_sdi);
      count := count + 1;
    end loop;
    check_equal(count, n_bits, result("for sample_sdi count"));
  end process;

  p_check_sample_strobe_sdo_count : process
    alias sample_sdo is << signal e_dut.sample_sdo : std_ulogic >>;
    variable count                                 : integer := 0;
  begin
    wait until start = '1';

    count := 0;
    while not ready = '1' loop
      wait until rising_edge(sample_sdo);
      count := count + 1;
    end loop;
    check_equal(count, n_bits, result("for sample_sdo count"));
  end process;

  p_check_sample_strobe_edge : process
    alias sample_sdi is << signal e_dut.sample_sdi : std_ulogic >>;
    alias sample_sdo is << signal e_dut.sample_sdo : std_ulogic >>;
  begin
    wait until start = '1';

    while not ready = '1' loop
      -- the edge away from the idle state is always the first edge
      wait on sclk;
      if transmit_on_sclk_edge_toward_idle_state = '1' then
        check_equal(sample_sdi, '1', result("for sample_sdi"));
        check_equal(sample_sdo, '0', result("for sample_sdo"));
      else
        check_equal(sample_sdi, '0', result("for sample_sdi"));
        check_equal(sample_sdo, '1', result("for sample_sdo"));
      end if;

      -- the edge toward the idle state is always the second edge
      wait on sclk;
      if transmit_on_sclk_edge_toward_idle_state = '1' then
        check_equal(sample_sdi, '0', result("for sample_sdi"));
        check_equal(sample_sdo, '1', result("for sample_sdo"));
      else
        check_equal(sample_sdi, '1', result("for sample_sdi"));
        check_equal(sample_sdo, '0', result("for sample_sdo"));
      end if;
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

  p_check_data_from_peripheral : process
  begin
    wait until start = '1';

    for n in n_bits - 1 downto 0 loop
      sd_from_peripheral <= d_from_peripheral_expected(n);
      if transmit_on_sclk_edge_toward_idle_state = '1' then
        wait_until_sclk_edge_away_from_idle(sclk, sclk_idle_state);
      else
        wait_until_sclk_edge_toward_idle(sclk, sclk_idle_state);
      end if;
      WaitForClock(clk, 1);
    end loop;

    wait until ready = '1';
    check_equal(d_from_peripheral, d_from_peripheral_expected, result("for d_from_peripheral"));
  end process;

end architecture;

