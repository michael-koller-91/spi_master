library ieee;
  use ieee.math_real.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_1164.all;

library vunit_lib;
  context vunit_lib.vunit_context;

library osvvm;
  use osvvm.tbutilpkg.all;
  use osvvm.randompkg.all;

library spi_lib;
  use spi_lib.spi_package.all;

entity tb_spi_master is
  generic (
    runner_cfg                           : string;
    g_rng_seed                           : integer    := 2;
    g_sclk_idle_state                    : std_ulogic := '1';
    g_scs_idle_state                     : std_ulogic := '1';
    g_transmit_on_sclk_leading_edge      : std_ulogic := '1';
    g_max_n_clks_scs_to_sclk             : positive   := 3;
    g_max_n_clks_sclk_to_scs             : positive   := 5;
    g_max_n_bits                         : positive   := 4;
    g_max_sclk_divide_half               : positive   := 4;
    g_max_n_clks_sclk_to_le              : positive   := 2;
    g_max_n_clks_le_width                : positive   := 3;
    g_max_n_clks_rx_sample_strobes_delay : natural    := 0
  );
end entity tb_spi_master;

architecture arch of tb_spi_master is

  constant c_clk_period : time     := 10 ns;
  constant c_config     : t_config :=
  (
    max_n_clks_scs_to_sclk             => g_max_n_clks_scs_to_sclk,
    max_n_clks_sclk_to_scs             => g_max_n_clks_sclk_to_scs,
    max_n_bits                         => g_max_n_bits,
    max_sclk_divide_half               => g_max_sclk_divide_half,
    max_n_clks_sclk_to_le              => g_max_n_clks_sclk_to_le,
    max_n_clks_le_width                => g_max_n_clks_le_width,
    max_n_clks_rx_sample_strobes_delay => g_max_n_clks_rx_sample_strobes_delay
  );

  procedure wait_until_sclk_leading_edge (
    signal o_sclk          : std_ulogic;
    signal sclk_idle_state : std_ulogic
  ) is
  begin

    if (sclk_idle_state = '0') then
      wait until rising_edge(o_sclk);
    else
      wait until falling_edge(o_sclk);
    end if;

  end procedure;

  procedure wait_until_sclk_trailing_edge (
    signal o_sclk          : std_ulogic;
    signal sclk_idle_state : std_ulogic
  ) is
  begin

    if (sclk_idle_state = '0') then
      wait until falling_edge(o_sclk);
    else
      wait until rising_edge(o_sclk);
    end if;

  end procedure;

  signal clk : std_ulogic := '0';

  signal d_from_peripheral               : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal d_from_peripheral_expected      : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal o_d_from_peripheral_read_strobe : std_ulogic                                          := '0';
  signal d_to_peripheral                 : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');

  signal start                : std_ulogic := '0';
  signal i_sd_from_peripheral : std_ulogic := '0';
  signal o_sd_to_peripheral   : std_ulogic := '0';

  signal keep_streaming  : std_ulogic := '0';
  signal streaming_start : std_ulogic := '0';

  signal busy            : std_ulogic := '0';
  signal ready           : std_ulogic := '0';
  signal ready_reference : std_ulogic := '0';
  signal o_sclk          : std_ulogic := '0';
  signal sclk_reference  : std_ulogic := '0';
  signal o_scs           : std_ulogic := '0';
  signal scs_reference   : std_ulogic := '0';
  signal le              : std_ulogic := '0';
  signal le_reference    : std_ulogic := '0';

  signal sclk_divide_half               : natural range 1 to c_config.max_sclk_divide_half               := 1;
  signal n_bits                         : natural range 1 to c_config.max_n_bits                         := 1;
  signal n_clks_scs_to_sclk             : natural range 1 to c_config.max_n_clks_scs_to_sclk             := 1;
  signal n_clks_sclk_to_scs             : natural range 1 to c_config.max_n_clks_sclk_to_scs             := 1;
  signal n_clks_sclk_to_le              : natural range 1 to c_config.max_n_clks_sclk_to_le              := 1;
  signal n_clks_le_width                : natural range 1 to c_config.max_n_clks_le_width                := 1;
  signal n_clks_rx_sample_strobes_delay : natural range 0 to c_config.max_n_clks_rx_sample_strobes_delay := 0;

  signal sclk_idle_state               : std_ulogic := g_sclk_idle_state;
  signal scs_idle_state                : std_ulogic := g_scs_idle_state;
  signal transmit_on_sclk_leading_edge : std_ulogic := g_transmit_on_sclk_leading_edge;
  signal streaming_mode                : std_ulogic := '0';

  signal settings : t_settings
        (
          sclk_divide_half_minus_1 (ceil_log2(c_config.max_sclk_divide_half) - 1 downto 0),
          n_bits_minus_1 (ceil_log2(c_config.max_n_bits) - 1 downto 0),
          n_clks_scs_to_sclk_minus_1 (ceil_log2(c_config.max_n_clks_scs_to_sclk) - 1 downto 0),
          n_clks_sclk_to_scs_minus_1 (ceil_log2(c_config.max_n_clks_sclk_to_scs) - 1 downto 0),
          n_clks_sclk_to_le_minus_1 (ceil_log2(c_config.max_n_clks_sclk_to_le) - 1 downto 0),
          n_clks_le_width_minus_1 (ceil_log2(c_config.max_n_clks_le_width) - 1 downto 0),
          n_clks_rx_sample_strobes_delay (ceil_log2(c_config.max_n_clks_rx_sample_strobes_delay + 1) - 1 downto 0)
        );

  signal level : log_level_t := error; -- If this es equal to `warning`, tests don't fail on `error`.

begin

  settings.scs_idle_state                 <= scs_idle_state;
  settings.sclk_idle_state                <= sclk_idle_state;
  settings.transmit_on_sclk_leading_edge  <= transmit_on_sclk_leading_edge;
  settings.streaming_mode                 <= streaming_mode;
  settings.sclk_divide_half_minus_1       <= to_unsigned(sclk_divide_half - 1, ceil_log2(c_config.max_sclk_divide_half));
  settings.n_bits_minus_1                 <= to_unsigned(n_bits - 1, ceil_log2(c_config.max_n_bits));
  settings.n_clks_scs_to_sclk_minus_1     <= to_unsigned(n_clks_scs_to_sclk - 1, ceil_log2(c_config.max_n_clks_scs_to_sclk));
  settings.n_clks_sclk_to_scs_minus_1     <= to_unsigned(n_clks_sclk_to_scs - 1, ceil_log2(c_config.max_n_clks_sclk_to_scs));
  settings.n_clks_sclk_to_le_minus_1      <= to_unsigned(n_clks_sclk_to_le - 1, ceil_log2(c_config.max_n_clks_sclk_to_le));
  settings.n_clks_le_width_minus_1        <= to_unsigned(n_clks_le_width - 1, ceil_log2(c_config.max_n_clks_le_width));
  settings.n_clks_rx_sample_strobes_delay <= to_unsigned(n_clks_rx_sample_strobes_delay, ceil_log2(c_config.max_n_clks_rx_sample_strobes_delay + 1));

  e_dut : entity spi_lib.spi_master(arch)
    generic map (
      g_config => c_config
    )
    port map (
      i_clk             => clk,
      i_start           => start,
      o_busy            => busy,
      o_ready           => ready,
      i_keep_streaming  => keep_streaming,
      o_streaming_start => streaming_start,
      --
      i_d_to_peripheral               => d_to_peripheral,
      o_d_from_peripheral             => d_from_peripheral,
      o_d_from_peripheral_read_strobe => o_d_from_peripheral_read_strobe,
      --
      i_settings => settings,
      --
      o_le                 => le,
      o_scs                => o_scs,
      o_sclk               => o_sclk,
      o_sd_to_peripheral   => o_sd_to_peripheral,
      i_sd_from_peripheral => i_sd_from_peripheral
    );

  CreateClock(clk, c_clk_period);

  main : process is

    variable rv : RandomPType;

  begin

    test_runner_setup(runner, runner_cfg);

    set_stop_level(default_logger, failure);

    RV.InitSeed(g_rng_seed);
    info("g_rng_seed = " & to_string(g_rng_seed));

    --
    -- by default, use the testbench generics
    --
    scs_idle_state                 <= g_scs_idle_state;
    sclk_idle_state                <= g_sclk_idle_state;
    transmit_on_sclk_leading_edge  <= g_transmit_on_sclk_leading_edge;
    sclk_divide_half               <= c_config.max_sclk_divide_half;
    n_bits                         <= c_config.max_n_bits;
    n_clks_scs_to_sclk             <= c_config.max_n_clks_scs_to_sclk;
    n_clks_sclk_to_scs             <= c_config.max_n_clks_sclk_to_scs;
    n_clks_sclk_to_le              <= c_config.max_n_clks_sclk_to_le;
    n_clks_le_width                <= c_config.max_n_clks_le_width;
    n_clks_rx_sample_strobes_delay <= c_config.max_n_clks_rx_sample_strobes_delay;
    --
    WaitForClock(clk, 1);
    --
    info("-- begin init");
    info("scs_idle_state = " & to_string(scs_idle_state));
    info("sclk_idle_state = " & to_string(sclk_idle_state));
    info("transmit_on_sclk_leading_edge = " & to_string(transmit_on_sclk_leading_edge));
    info("sclk_divide_half = " & to_string(sclk_divide_half));
    info("n_bits = " & to_string(n_bits));
    info("n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));
    info("n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
    info("n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
    info("n_clks_le_width = " & to_string(n_clks_le_width));
    info("n_clks_rx_sample_strobes_delay = " & to_string(n_clks_rx_sample_strobes_delay));
    info("-- end init");

    for k in d_to_peripheral'range loop

      if (k mod 2 = 0) then
        d_to_peripheral(k) <= '0';
      else
        d_to_peripheral(k) <= '1';
      end if;

    end loop;

    WaitForClock(clk, 10);

    info("d_to_peripheral = " & to_string(d_to_peripheral));

    while test_suite loop

      if run("01_all_sclk_scs_idle_cases") then
        start <= '1';
        WaitForClock(clk, 1);
        start <= '0';

        WaitForClock(clk, 1);
        -- check_equal(ready, '0', result("for ready"));
        wait until rising_edge(ready);
        WaitForClock(clk, 2);
      -- check_equal(ready, '0', result("for ready"));
      elsif run("02_all_sclk_transmit_edge_cases") then
        start <= '1';
        WaitForClock(clk, 1);
        start <= '0';

        WaitForClock(clk, 1);
        -- check_equal(ready, '0', result("for ready"));

        wait until ready = '1';
      elsif run("03_sclk_divide") then

        for divide_half in 1 to c_config.max_sclk_divide_half loop

          sclk_divide_half <= divide_half;
          WaitForClock(clk, 1);
          info("sclk_divide_half = " & to_string(sclk_divide_half));

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          WaitForClock(clk, 1);
          -- check_equal(ready, '0', result("for ready"));

          wait until ready = '1';

          WaitForClock(clk, 5);

        end loop;

      elsif run("04_n_bits") then

        for bits in 1 to c_config.max_n_bits loop

          n_bits <= bits;
          WaitForClock(clk, 1);
          info("n_bits = " & to_string(n_bits));

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          WaitForClock(clk, 1);
          -- check_equal(ready, '0', result("for ready"));

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

        for clks_scs_to_sclk in 1 to c_config.max_n_clks_scs_to_sclk loop

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

        for clks_sclk_to_scs in 1 to c_config.max_n_clks_sclk_to_scs loop

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

        for clks_rx_sample_strobes_delay in 0 to 0 loop -- c_config.max_n_clks_rx_sample_strobes_delay loop

          n_clks_rx_sample_strobes_delay <= clks_rx_sample_strobes_delay;
          d_from_peripheral_expected     <= (others => '1');

          WaitForClock(clk, 1);
          start <= '1';

          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);

        end loop;

        info("random");

        for clks_rx_sample_strobes_delay in 0 to c_config.max_n_clks_rx_sample_strobes_delay loop

          d_from_peripheral_expected <= RV.RandSlv(d_from_peripheral_expected'length);
          WaitForClock(clk, 1);

          start <= '1';
          WaitForClock(clk, 1);
          start <= '0';

          wait until ready = '1';

          WaitForClock(clk, 5);

        end loop;

      elsif run("10_streaming_mode") then
        streaming_mode <= '1';
        keep_streaming <= '1';

        WaitForClock(clk, 1);

        start <= '1';
        WaitForClock(clk, 1);
        start <= '0';

        WaitForClock(clk, 100);
        keep_streaming <= '0';

        WaitForClock(clk, 100);
      end if;

    end loop;

    WaitForClock(clk, 10);

    test_runner_cleanup(runner);
    wait;

  end process main;

  test_runner_watchdog(runner, 20 us);

  ---------------------------------------------------------------------------
  -- check ready
  ---------------------------------------------------------------------------

  p_generate_ready_reference : process is

    variable n_wait            : integer := 0;
    variable n_wait_sample_sdi : integer := 0;

  begin

    wait until rising_edge(start);
    ready_reference <= '0';

    -- wait until o_sclk stops
    WaitForClock(clk, 1 + n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half);

    -- wait until o_scs and LE are ready
    n_wait := maximum(n_clks_sclk_to_scs, n_clks_sclk_to_le + n_clks_le_width);      -- either o_scs inactive or LE takes longer

    -- wait until the last sample has been sampled
    if (transmit_on_sclk_leading_edge = '1') then                                    -- receive on o_sclk away from idle state
      n_wait_sample_sdi := n_clks_rx_sample_strobes_delay - sclk_divide_half;
    else
      n_wait_sample_sdi := n_clks_rx_sample_strobes_delay;
    end if;

    n_wait := maximum(n_wait, n_wait_sample_sdi + 2);                                -- +1 for sampling +1 for ready

    WaitForClock(clk, n_wait);
    ready_reference <= '1';

    WaitForClock(clk, 1);
    ready_reference <= '0';

  end process p_generate_ready_reference;

  p_check_ready : process is
  begin

    wait on ready, ready_reference;
    wait for 0 fs;

    if (streaming_mode = '0') then
    -- check_equal(ready, ready_reference, "ready is not equal to ready_reference.");
    end if;

  end process p_check_ready;

  ---------------------------------------------------------------------------
  -- check o_sclk
  ---------------------------------------------------------------------------

  p_generate_sclk_reference : process is
  begin

    wait until rising_edge(start);
    sclk_reference <= sclk_idle_state;

    wait until falling_edge(start);
    WaitForClock(clk, 2);
    WaitForClock(clk, n_clks_scs_to_sclk);
    sclk_reference <= not sclk_reference;

    for k in 1 to 2 * n_bits - 1 loop

      WaitForClock(clk, sclk_divide_half);
      sclk_reference <= not sclk_reference;

    end loop;

  end process p_generate_sclk_reference;

  p_check_sclk : process is
  begin

    wait until falling_edge(start);

    while not rising_edge(ready) loop

      wait on ready, o_sclk, sclk_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_sclk, sclk_reference, "o_sclk is not equal to sclk_reference.", level => level);
      end if;

    end loop;

  end process p_check_sclk;

  ---------------------------------------------------------------------------
  -- check o_scs
  ---------------------------------------------------------------------------

  p_generate_scs_reference : process is
  begin

    wait until rising_edge(start);
    scs_reference <= scs_idle_state;

    wait until falling_edge(start);
    scs_reference <= not scs_idle_state;

    WaitForClock(clk, n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half + n_clks_sclk_to_scs);
    scs_reference <= scs_idle_state;

  end process p_generate_scs_reference;

  p_check_scs : process is
  begin

    wait until start = '1';

    while not ready = '1' loop

      wait on o_scs, scs_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
      -- check_equal(o_scs, scs_reference, "o_scs is not equal to scs_reference.");
      end if;

    end loop;

  end process p_check_scs;

  ---------------------------------------------------------------------------
  -- check LE
  ---------------------------------------------------------------------------

  p_generate_le_reference : process is
  begin

    wait until falling_edge(start);
    le_reference <= '0';

    WaitForClock(clk, n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half + n_clks_sclk_to_le);
    le_reference <= '1';

    WaitForClock(clk, n_clks_le_width);
    le_reference <= '0';

  end process p_generate_le_reference;

  p_check_le : process is
  begin

    wait until start = '1';

    while not ready = '1' loop

      wait on le, le_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
      -- check_equal(le, le_reference, "le is not equal to le_reference.");
      end if;

    end loop;

  end process p_check_le;

  ---------------------------------------------------------------------------
  -- check data
  ---------------------------------------------------------------------------

  p_check_data_to_peripheral : process is
  begin

    wait until start = '1';

    for n in d_to_peripheral'range loop

      if (transmit_on_sclk_leading_edge = '1') then
        wait_until_sclk_leading_edge(o_sclk, sclk_idle_state);
        check_equal(o_sd_to_peripheral, d_to_peripheral(n), result("for o_sd_to_peripheral"), level => level);
      else
        wait_until_sclk_trailing_edge(o_sclk, sclk_idle_state);
        check_equal(o_sd_to_peripheral, d_to_peripheral(n), result("for o_sd_to_peripheral"), level => level);
      end if;

    end loop;

  end process p_check_data_to_peripheral;

  p_generate_sd_from_peripheral : process is
  begin

    wait until start = '1';

    i_sd_from_peripheral <= '0';

    wait on o_sclk;

    WaitForClock(clk, n_clks_rx_sample_strobes_delay);

    for n in d_from_peripheral_expected'range loop

      i_sd_from_peripheral <= d_from_peripheral_expected(n);

      WaitForClock(clk, 1);
      i_sd_from_peripheral <= '0';

      WaitForClock(clk, 2 * sclk_divide_half - 1);

    end loop;

  end process p_generate_sd_from_peripheral;

  p_check_data_from_peripheral : process is
  begin

    wait until o_d_from_peripheral_read_strobe = '1';
    check_equal(d_from_peripheral, d_from_peripheral_expected, result("for d_from_peripheral"), level => level);

  end process p_check_data_from_peripheral;

end architecture arch;

