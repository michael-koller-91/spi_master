library ieee;
  use ieee.math_real.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_1164.all;

library vunit_lib;
  context vunit_lib.vunit_context;

library osvvm;
  context osvvm.osvvmcontext;

library spi_lib;
  use spi_lib.spi_package.all;
  use spi_lib.tb_package.all;

entity tb_spi_master is
  generic (
    runner_cfg                           : string;
    g_rng_seed                           : integer    := 34128;
    g_warning                            : boolean    := false;
    g_sclk_idle_state                    : std_ulogic := '1';
    g_scs_idle_state                     : std_ulogic := '1';
    g_transmit_on_sclk_leading_edge      : std_ulogic := '1';
    g_max_n_clks_scs_to_sclk             : positive   := 3;
    g_max_n_clks_sclk_to_scs             : positive   := 5;
    g_max_n_bits                         : positive   := 7;
    g_max_sclk_divide_half               : positive   := 8;
    g_max_n_clks_sclk_to_le              : positive   := 2;
    g_max_n_clks_le_width                : positive   := 3;
    g_max_n_clks_rx_sample_strobes_delay : positive    := 1
    -- g_watchdog_timeout                   : time       := 50 us
  );
end entity tb_spi_master;

architecture arch of tb_spi_master is
  constant g_watchdog_timeout : time := 50 ms;
  constant c_max_n_trx_loops : positive := 31;

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

  signal i_clk : std_ulogic := '0';

  signal o_d_from_peripheral             : t_d_from_peripheral(data(c_config.max_n_bits - 1 downto 0)) := ((others => '0'), '0');
  signal d_from_peripheral_expected      : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal n_trx_loops                     : integer                                             := 1;
  signal d_from_peripheral_expected_old  : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal o_sampled_d_to_peripheral   : std_ulogic                                          := '0';
  signal i_d_to_peripheral               : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');

  signal check_data_to_peripheral_done    : std_ulogic := '0';
  signal generate_sd_from_peripheral_done : std_ulogic := '0';

  signal i_start              : std_ulogic := '0';
  signal i_sd_from_peripheral : std_ulogic := '0';
  signal o_sd_to_peripheral   : std_ulogic := '0';

  signal i_keep_streaming  : std_ulogic := '0';

  signal o_busy          : std_ulogic := '0';
  signal o_ready         : std_ulogic := '0';
  signal ready_delayed1  : std_ulogic := '0';
  signal ready_delayed2  : std_ulogic := '0';
  signal ready_reference : std_ulogic := '0';
  signal o_sclk          : std_ulogic := '0';
  signal sclk_reference  : std_ulogic := '0';
  signal o_scs           : std_ulogic := '0';
  signal scs_reference   : std_ulogic := '0';
  signal o_le            : std_ulogic := '0';
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

  signal i_settings : t_settings
        (
          sclk_divide_half_minus_1 (ceil_log2(c_config.max_sclk_divide_half) - 1 downto 0),
          n_bits_minus_1 (ceil_log2(c_config.max_n_bits) - 1 downto 0),
          n_clks_scs_to_sclk_minus_1 (ceil_log2(c_config.max_n_clks_scs_to_sclk) - 1 downto 0),
          n_clks_sclk_to_scs_minus_1 (ceil_log2(c_config.max_n_clks_sclk_to_scs) - 1 downto 0),
          n_clks_sclk_to_le_minus_1 (ceil_log2(c_config.max_n_clks_sclk_to_le) - 1 downto 0),
          n_clks_le_width_minus_1 (ceil_log2(c_config.max_n_clks_le_width) - 1 downto 0),
          n_clks_rx_sample_strobes_delay (ceil_log2(c_config.max_n_clks_rx_sample_strobes_delay + 1) - 1 downto 0)
        );

  signal level : log_level_t := error; -- If this is equal to `warning`, tests don't fail on `error`. See `python run.py --help`.

  shared variable rv : RandomPType;

  signal coverage_id : CoverageIDType;

  shared variable counter_checks : t_counter_checks;

begin

  i_settings.scs_idle_state                 <= scs_idle_state;
  i_settings.sclk_idle_state                <= sclk_idle_state;
  i_settings.transmit_on_sclk_leading_edge  <= transmit_on_sclk_leading_edge;
  i_settings.sclk_divide_half_minus_1       <= to_unsigned(sclk_divide_half - 1, ceil_log2(c_config.max_sclk_divide_half));
  i_settings.n_bits_minus_1                 <= to_unsigned(n_bits - 1, ceil_log2(c_config.max_n_bits));
  i_settings.n_clks_scs_to_sclk_minus_1     <= to_unsigned(n_clks_scs_to_sclk - 1, ceil_log2(c_config.max_n_clks_scs_to_sclk));
  i_settings.n_clks_sclk_to_scs_minus_1     <= to_unsigned(n_clks_sclk_to_scs - 1, ceil_log2(c_config.max_n_clks_sclk_to_scs));
  i_settings.n_clks_sclk_to_le_minus_1      <= to_unsigned(n_clks_sclk_to_le - 1, ceil_log2(c_config.max_n_clks_sclk_to_le));
  i_settings.n_clks_le_width_minus_1        <= to_unsigned(n_clks_le_width - 1, ceil_log2(c_config.max_n_clks_le_width));
  i_settings.n_clks_rx_sample_strobes_delay <= to_unsigned(n_clks_rx_sample_strobes_delay, ceil_log2(c_config.max_n_clks_rx_sample_strobes_delay + 1));

  e_dut : entity spi_lib.spi_master(arch)
    generic map (
      g_config => c_config
    )
    port map (
      i_clk             => i_clk,
      i_start           => i_start,
      o_busy            => o_busy,
      o_ready           => o_ready,
      i_keep_streaming  => i_keep_streaming,
      --
      i_d_to_peripheral         => i_d_to_peripheral,
      o_sampled_d_to_peripheral => o_sampled_d_to_peripheral,
      o_d_from_peripheral       => o_d_from_peripheral,
      --
      i_settings => i_settings,
      --
      o_le                 => o_le,
      o_scs                => o_scs,
      o_sclk               => o_sclk,
      o_sd_to_peripheral   => o_sd_to_peripheral,
      i_sd_from_peripheral => i_sd_from_peripheral
    );

  CreateClock(i_clk, c_clk_period);

  main : process is

    variable counter_coverage                               : integer := 0;
    variable tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8 : integer;
    constant n_bins                                         : integer := 3;

  begin

    test_runner_setup(runner, runner_cfg);
    info("g_watchdog_timeout = " & to_string(g_watchdog_timeout));

    set_stop_level(default_logger, failure);

    rv.InitSeed(g_rng_seed);
    info("g_rng_seed = " & to_string(g_rng_seed));

    if (g_warning) then
      level <= warning;
    end if;

    WaitForClock(i_clk, 1);
    info("level = " & to_string(level));

    while test_suite loop

      if run("00_simple_case_for_debugging") then
        sclk_divide_half               <= c_config.max_sclk_divide_half;
        n_bits                         <= c_config.max_n_bits;
        n_clks_scs_to_sclk             <= c_config.max_n_clks_scs_to_sclk;
        n_clks_sclk_to_scs             <= c_config.max_n_clks_sclk_to_scs;
        n_clks_sclk_to_le              <= c_config.max_n_clks_sclk_to_le;
        n_clks_le_width                <= c_config.max_n_clks_le_width;
        n_clks_rx_sample_strobes_delay <= c_config.max_n_clks_rx_sample_strobes_delay;
        scs_idle_state                 <= g_scs_idle_state;
        sclk_idle_state                <= g_sclk_idle_state;
        transmit_on_sclk_leading_edge  <= g_transmit_on_sclk_leading_edge;

        WaitForClock(i_clk, 1);

        info("settings:");
        info("  sclk_divide_half = " & to_string(sclk_divide_half));
        info("  n_bits = " & to_string(n_bits));
        info("  n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));
        info("  n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
        info("  n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
        info("  n_clks_le_width = " & to_string(n_clks_le_width));
        info("  n_clks_rx_sample_strobes_delay = " & to_string(n_clks_rx_sample_strobes_delay));
        info("  scs_idle_state = " & to_string(scs_idle_state));
        info("  sclk_idle_state = " & to_string(sclk_idle_state));
        info("  transmit_on_sclk_leading_edge = " & to_string(transmit_on_sclk_leading_edge));

        i_start <= '1';
        WaitForClock(i_clk, 1);
        i_start <= '0';

        wait until o_ready = '1';

        WaitForClock(i_clk, 10);
      elsif run("01_random_coverage") then
        counter_checks.reset;

        coverage_id <= NewID("coverage_id");
        wait for 0 fs;

        AddCross(
                 coverage_id,
                 GenBin(1, c_config.max_sclk_divide_half, n_bins),
                 GenBin(1, c_config.max_n_bits, n_bins),
                 GenBin(1, c_config.max_n_clks_scs_to_sclk, n_bins),
                 GenBin(1, c_config.max_n_clks_sclk_to_scs, n_bins),
                 GenBin(1, c_config.max_n_clks_sclk_to_le, n_bins),
                 GenBin(1, c_config.max_n_clks_le_width, n_bins),
                 GenBin(0, c_config.max_n_clks_rx_sample_strobes_delay, n_bins)
               );

        counter_coverage := 0;

        while not IsCovered(coverage_id) loop

          counter_coverage := counter_coverage + 1;

          (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) := GetRandPoint(coverage_id);

          sclk_divide_half               <= tmp1;
          n_bits                         <= tmp2;
          n_clks_scs_to_sclk             <= tmp3;
          n_clks_sclk_to_scs             <= tmp4;
          n_clks_sclk_to_le              <= tmp5;
          n_clks_le_width                <= tmp6;
          n_clks_rx_sample_strobes_delay <= tmp7;
          scs_idle_state                 <= rv.RandSlv(1)(1);
          sclk_idle_state                <= rv.RandSlv(1)(1);
          transmit_on_sclk_leading_edge  <= rv.RandSlv(1)(1);

          WaitForClock(i_clk, 1);

          info("counter_coverage = " & to_string(counter_coverage));
          info("    sclk_divide_half = " & to_string(sclk_divide_half));
          info("    n_bits = " & to_string(n_bits));
          info("    n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));
          info("    n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
          info("    n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
          info("    n_clks_le_width = " & to_string(n_clks_le_width));
          info("    n_clks_rx_sample_strobes_delay = " & to_string(n_clks_rx_sample_strobes_delay));
          info("    scs_idle_state = " & to_string(scs_idle_state));
          info("    sclk_idle_state = " & to_string(sclk_idle_state));
          info("    transmit_on_sclk_leading_edge = " & to_string(transmit_on_sclk_leading_edge));

          WaitForClock(i_clk, 5);

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 2 * 50 + 10); -- because `upper = 50` in run.py

          ICover(coverage_id, (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7));

        end loop;

        counter_checks.print_values;
        counter_checks.all_equal_to(counter_coverage);
      elsif run("10_streaming_mode_simple_case_for_debugging") then
        streaming_mode   <= '1';
        i_keep_streaming <= '1';
        wait for 0 ns;

        sclk_divide_half               <= c_config.max_sclk_divide_half;
        n_bits                         <= c_config.max_n_bits;
        n_clks_scs_to_sclk             <= c_config.max_n_clks_scs_to_sclk;
        n_clks_sclk_to_scs             <= c_config.max_n_clks_sclk_to_scs;
        n_clks_sclk_to_le              <= c_config.max_n_clks_sclk_to_le;
        n_clks_le_width                <= c_config.max_n_clks_le_width;
        n_clks_rx_sample_strobes_delay <= c_config.max_n_clks_rx_sample_strobes_delay;
        n_trx_loops                    <= 6;
        scs_idle_state                 <= g_scs_idle_state;
        sclk_idle_state                <= g_sclk_idle_state;
        transmit_on_sclk_leading_edge  <= g_transmit_on_sclk_leading_edge;

        WaitForClock(i_clk, 1);

        info("settings:");
        info("  sclk_divide_half = " & to_string(sclk_divide_half));
        info("  n_bits = " & to_string(n_bits));
        info("  n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));
        info("  n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
        info("  n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
        info("  n_clks_le_width = " & to_string(n_clks_le_width));
        info("  n_clks_rx_sample_strobes_delay = " & to_string(n_clks_rx_sample_strobes_delay));
        info("  n_trx_loops = " & to_string(n_trx_loops));
        info("  scs_idle_state = " & to_string(scs_idle_state));
        info("  sclk_idle_state = " & to_string(sclk_idle_state));
        info("  transmit_on_sclk_leading_edge = " & to_string(transmit_on_sclk_leading_edge));

        i_start <= '1';
        WaitForClock(i_clk, 1);
        i_start <= '0';

        for k in 1 to n_trx_loops - 2 loop

          wait until rising_edge(o_sampled_d_to_peripheral);
          WaitForClock(i_clk, 1);

          if (k = n_trx_loops - 2) then
            i_keep_streaming <= '0';
          end if;

        end loop;

        wait until rising_edge(o_sampled_d_to_peripheral);
        WaitForClock(i_clk, 1);

        WaitForClock(i_clk, 30);
      elsif run("11_streaming_mode_random_coverage") then
        streaming_mode <= '1';
        wait for 0 ns;

        counter_checks.reset;

        coverage_id <= NewID("coverage_id");
        wait for 0 fs;

        AddCross(
                 coverage_id,
                 GenBin(1, c_config.max_sclk_divide_half, n_bins),
                 GenBin(1, c_config.max_n_bits, n_bins),
                 GenBin(1, c_config.max_n_clks_scs_to_sclk, n_bins),
                 GenBin(1, c_config.max_n_clks_sclk_to_scs, n_bins),
                 GenBin(1, c_config.max_n_clks_sclk_to_le, n_bins),
                 GenBin(1, c_config.max_n_clks_le_width, n_bins),
                 GenBin(0, c_config.max_n_clks_rx_sample_strobes_delay, n_bins),
                 GenBin(3, c_max_n_trx_loops, n_bins)
               );

        counter_coverage := 0;

        while not IsCovered(coverage_id) loop

          counter_coverage := counter_coverage + 1;

          (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) := GetRandPoint(coverage_id);

          sclk_divide_half               <= 1;--tmp1;
          n_bits                         <= tmp2;
          n_clks_scs_to_sclk             <= tmp3;
          n_clks_sclk_to_scs             <= tmp4;
          n_clks_sclk_to_le              <= tmp5;
          n_clks_le_width                <= tmp6;
          n_clks_rx_sample_strobes_delay <= tmp7;
          n_trx_loops                    <= tmp8;
          scs_idle_state                 <= rv.RandSlv(1)(1);
          sclk_idle_state                <= rv.RandSlv(1)(1);
          transmit_on_sclk_leading_edge  <= '1';--rv.RandSlv(1)(1);

          if counter_coverage = 2 then
            sclk_divide_half <= 1;
          end if;

          WaitForClock(i_clk, 1);

          info("counter_coverage = " & to_string(counter_coverage));
          info("    sclk_divide_half = " & to_string(sclk_divide_half));
          info("    n_bits = " & to_string(n_bits));
          info("    n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));
          info("    n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
          info("    n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
          info("    n_clks_le_width = " & to_string(n_clks_le_width));
          info("    n_clks_rx_sample_strobes_delay = " & to_string(n_clks_rx_sample_strobes_delay));
          info("    n_trx_loops = " & to_string(n_trx_loops));
          info("    scs_idle_state = " & to_string(scs_idle_state));
          info("    sclk_idle_state = " & to_string(sclk_idle_state));
          info("    transmit_on_sclk_leading_edge = " & to_string(transmit_on_sclk_leading_edge));

          i_keep_streaming              <= '1';
          WaitForClock(i_clk, 5);

          counter_checks.inc_n_trx(n_trx_loops);

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          for k in 1 to n_trx_loops - 2 loop

            wait until rising_edge(o_sampled_d_to_peripheral);
            WaitForClock(i_clk, 1);

            if (k = n_trx_loops - 2) then
              i_keep_streaming <= '0';
            end if;

          end loop;

          wait until o_ready = '1';

          WaitForClock(i_clk, 2 * 50 + 10); -- because `upper = 50` in run.py

          counter_checks.print_values;

          exit when counter_coverage = 1;

          ICover(coverage_id, (tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8));
        end loop;

        counter_checks.print_values;
        counter_checks.streaming_mode_all_equal_to(counter_coverage);

      end if;

    end loop;

    WaitForClock(i_clk, 10);

    test_runner_cleanup(runner);
    wait;

  end process main;

  test_runner_watchdog(runner, g_watchdog_timeout);

  ---------------------------------------------------------------------------
  -- check o_ready
  ---------------------------------------------------------------------------

  p_generate_ready_reference : process is

    variable n_wait            : integer := 0;
    variable n_wait_sample_sdi : integer := 0;

  begin

    wait until rising_edge(i_start);
    ready_reference <= '0';

    -- wait until o_sclk stops
    WaitForClock(i_clk, 2 + n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half);

    -- wait until o_scs and o_le are o_ready
    -- either o_scs inactive or o_le takes longer
    -- info("n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));
    -- info("n_clks_sclk_to_le = " & to_string(n_clks_sclk_to_le));
    -- info("n_clks_le_width = " & to_string(n_clks_le_width));
    n_wait := maximum(n_clks_sclk_to_scs, n_clks_sclk_to_le + n_clks_le_width);
    -- info("n_wait = " & to_string(n_wait));

    -- wait until the last sample has been sampled
    n_wait_sample_sdi := n_clks_rx_sample_strobes_delay - sclk_divide_half + 3;
    -- info("n_wait_sample_sdi = " & to_string(n_wait_sample_sdi));

    n_wait := maximum(n_wait, n_wait_sample_sdi);

    WaitForClock(i_clk, n_wait);
    ready_reference <= '1';

    WaitForClock(i_clk, 1);
    ready_reference <= '0';

  end process p_generate_ready_reference;

  p_check_ready : process is

    variable last_check : boolean := false;

  begin

    wait until i_start = '1';

    while True loop

      wait on o_ready, ready_reference;

      if falling_edge(ready_reference) then
        last_check := true;
      end if;

      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_ready, ready_reference, "o_ready is not equal to ready_reference.", level => level);
      end if;

      exit when last_check;

    end loop;

    counter_checks.inc_ready;

  end process p_check_ready;

  ---------------------------------------------------------------------------
  -- check o_sclk
  ---------------------------------------------------------------------------

  p_generate_sclk_reference : process is
  begin

    wait until rising_edge(i_start);
    sclk_reference <= sclk_idle_state;

    wait until falling_edge(i_start);
    WaitForClock(i_clk, 1); -- scs goes active one clock after start, sclk goes active at least 1 clock after that
    WaitForClock(i_clk, n_clks_scs_to_sclk);
    sclk_reference <= not sclk_reference;

    for k in 1 to 2 * n_bits - 1 loop

      WaitForClock(i_clk, sclk_divide_half);
      sclk_reference <= not sclk_reference;

    end loop;

  end process p_generate_sclk_reference;

  p_check_sclk : process is
  begin

    wait until falling_edge(i_start);

    while True loop

      wait on o_ready, o_sclk, sclk_reference;
      exit when rising_edge(o_ready);
      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_sclk, sclk_reference, "o_sclk is not equal to sclk_reference.", level => level);
      end if;

    end loop;

    counter_checks.inc_sclk;

  end process p_check_sclk;

  ---------------------------------------------------------------------------
  -- check o_scs
  ---------------------------------------------------------------------------

  p_generate_scs_reference : process is
  begin

    wait until rising_edge(i_start);
    scs_reference <= scs_idle_state;

    wait until falling_edge(i_start);
    WaitForClock(i_clk, 1); -- due to output register
    scs_reference <= not scs_idle_state;

    WaitForClock(i_clk, n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half + n_clks_sclk_to_scs);
    scs_reference <= scs_idle_state;

  end process p_generate_scs_reference;

  p_check_scs : process is
  begin

    wait until i_start = '1';

    while not o_ready = '1' loop

      wait on o_ready, o_scs, scs_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_scs, scs_reference, "o_scs is not equal to scs_reference.", level => level);
      end if;

    end loop;

    counter_checks.inc_scs;

  end process p_check_scs;

  ---------------------------------------------------------------------------
  -- check LE
  ---------------------------------------------------------------------------

  p_generate_le_reference : process is
  begin

    wait until falling_edge(i_start);
    WaitForClock(i_clk, 1); -- due to output register
    le_reference <= '0';

    WaitForClock(i_clk, n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half + n_clks_sclk_to_le);
    le_reference <= '1';

    WaitForClock(i_clk, n_clks_le_width);
    le_reference <= '0';

  end process p_generate_le_reference;

  p_check_le : process is
  begin

    wait until i_start = '1';

    while not o_ready = '1' loop

      wait on o_ready, o_le, le_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_le, le_reference, "o_le is not equal to le_reference.", level => level);
      end if;

    end loop;

    counter_checks.inc_le;

  end process p_check_le;

  ---------------------------------------------------------------------------
  -- check data
  ---------------------------------------------------------------------------

  p_check_data_to_peripheral : process is
  begin

    check_data_to_peripheral_done <= '1';

    wait until rising_edge(i_start);

    check_data_to_peripheral_done <= '0';

    i_d_to_peripheral <= rv.RandSlv(i_d_to_peripheral'length);
    wait for 0 ps;

    for k in 1 to n_trx_loops loop

      info("  DUT -> TB (k = " & to_string(k) & "): i_d_to_peripheral = " & to_string(i_d_to_peripheral));

      for n in i_d_to_peripheral'left downto i_d_to_peripheral'left - n_bits + 1 loop

        if (transmit_on_sclk_leading_edge = '1') then
          wait_until_sclk_leading_edge(o_sclk, sclk_idle_state);
          check_equal(o_sd_to_peripheral, i_d_to_peripheral(n), result("for o_sd_to_peripheral (n = " & to_string(n) & ")"), level => level);
        else
          wait_until_sclk_trailing_edge(o_sclk, sclk_idle_state);
          check_equal(o_sd_to_peripheral, i_d_to_peripheral(n), result("for o_sd_to_peripheral (n = " & to_string(n) & ")"), level => level);
        end if;

      end loop;

      i_d_to_peripheral <= rv.RandSlv(i_d_to_peripheral'length);
      wait for 0 fs;

      counter_checks.inc_sd_to_peripheral;

    end loop;

  end process p_check_data_to_peripheral;

  p_generate_sd_from_peripheral : process is
  begin

    generate_sd_from_peripheral_done <= '1';

    wait until rising_edge(i_start);

    generate_sd_from_peripheral_done <= '0';

    d_from_peripheral_expected                      <= (others => '0');
    d_from_peripheral_expected(n_bits - 1 downto 0) <= rv.RandSlv(n_bits);

    i_sd_from_peripheral <= '0';

    wait until falling_edge(i_start);
    wait on o_sclk;

    WaitForClock(i_clk, n_clks_rx_sample_strobes_delay);

    for k in 1 to n_trx_loops loop

      info("  TB -> DUT (k = " & to_string(k) & "): d_from_peripheral_expected " & to_string(d_from_peripheral_expected));

      for n in n_bits - 1 downto 0 loop

        i_sd_from_peripheral <= d_from_peripheral_expected(n);

        WaitForClock(i_clk, 1);
        i_sd_from_peripheral <= '0';

        if (n > 0) then
          WaitForClock(i_clk, 2 * sclk_divide_half - 1);
        else
          -- This allows `d_from_peripheral_expected` to be updated early enough for the next round.
          WaitForClock(i_clk, 2 * sclk_divide_half - 2);
        end if;

      end loop;

      d_from_peripheral_expected                      <= (others => '0');
      d_from_peripheral_expected(n_bits - 1 downto 0) <= rv.RandSlv(n_bits);
      WaitForClock(i_clk, 1);

    end loop;

  end process p_generate_sd_from_peripheral;

  d_from_peripheral_expected_old <= transport d_from_peripheral_expected after 4 * c_clk_period;

  -- This is how a module receiving from `spi_master` is supposed to use the out port `o_d_from_peripheral`.
  p_check_data_from_peripheral : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (o_d_from_peripheral.valid) then
        check_equal(o_d_from_peripheral.data, d_from_peripheral_expected_old, result("for o_d_from_peripheral"), level => level);
        counter_checks.inc_sd_from_peripheral;
      end if;
    end if;

  end process p_check_data_from_peripheral;

end architecture arch;

