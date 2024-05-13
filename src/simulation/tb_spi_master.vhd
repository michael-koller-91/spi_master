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
    g_warning                            : boolean    := false;
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

  signal i_clk : std_ulogic := '0';

  signal o_d_from_peripheral             : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal d_from_peripheral_expected      : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal n_trx_loops                     : integer                                             := 1;
  signal d_from_peripheral_expected_old  : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal o_d_from_peripheral_read_strobe : std_ulogic                                          := '0';
  signal o_d_to_peripheral_read_strobe   : std_ulogic                                          := '0';
  signal i_d_to_peripheral               : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');
  signal i_d_to_peripheral_expected      : std_ulogic_vector(c_config.max_n_bits - 1 downto 0) := (others => '0');

  signal check_o_d_from_peripheral  : std_ulogic := '0';
  signal n_checks_d_from_peripheral : integer    := 0;

  signal i_start              : std_ulogic := '0';
  signal i_sd_from_peripheral : std_ulogic := '0';
  signal o_sd_to_peripheral   : std_ulogic := '0';

  signal o_streaming_start : std_ulogic := '0';
  signal i_keep_streaming  : std_ulogic := '0';

  signal o_busy          : std_ulogic := '0';
  signal o_ready         : std_ulogic := '0';
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
      o_streaming_start => o_streaming_start,
      i_keep_streaming  => i_keep_streaming,
      --
      i_d_to_peripheral               => i_d_to_peripheral,
      o_d_from_peripheral             => o_d_from_peripheral,
      o_d_from_peripheral_read_strobe => o_d_from_peripheral_read_strobe,
      o_d_to_peripheral_read_strobe   => o_d_to_peripheral_read_strobe,
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
  -- variable rv : RandomPType;
  begin

    test_runner_setup(runner, runner_cfg);

    set_stop_level(default_logger, failure);

    rv.InitSeed(g_rng_seed);
    info("g_rng_seed = " & to_string(g_rng_seed));

    if (g_warning) then
      level <= warning;
    end if;

    WaitForClock(i_clk, 1);
    info("level = " & to_string(level));

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
    WaitForClock(i_clk, 1);
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

    for k in i_d_to_peripheral'range loop

      if (k mod 2 = 0) then
        i_d_to_peripheral(k) <= '0';
      else
        i_d_to_peripheral(k) <= '1';
      end if;

    end loop;

    WaitForClock(i_clk, 5);
    i_d_to_peripheral_expected <= i_d_to_peripheral;
    WaitForClock(i_clk, 5);

    info("i_d_to_peripheral = " & to_string(i_d_to_peripheral));

    while test_suite loop

      if run("00_simple_case_for_debugging") then
        sclk_idle_state <= '0';
        scs_idle_state  <= '0';
        WaitForClock(i_clk, 10);

        i_start <= '1';

        WaitForClock(i_clk, 1);
        i_start <= '0';

        WaitForClock(i_clk, 1);
        check_equal(o_ready, '0', result("for o_ready"), level => level);

        wait until o_ready = '1';
      elsif run("01_all_sclk_scs_idle_cases") then
        i_start <= '1';

        WaitForClock(i_clk, 1);
        i_start <= '0';

        WaitForClock(i_clk, 1);
        check_equal(o_ready, '0', result("for o_ready"), level => level);

        wait until rising_edge(o_ready);

        WaitForClock(i_clk, 2);
        check_equal(o_ready, '0', result("for o_ready"), level => level);
      elsif run("02_all_sclk_transmit_edge_cases") then
        i_start <= '1';

        WaitForClock(i_clk, 1);
        i_start <= '0';

        WaitForClock(i_clk, 1);
        check_equal(o_ready, '0', result("for o_ready"), level => level);

        wait until o_ready = '1';
      elsif run("03_sclk_divide") then

        for divide_half in 1 to c_config.max_sclk_divide_half loop

          sclk_divide_half <= divide_half;
          WaitForClock(i_clk, 1);
          info("sclk_divide_half = " & to_string(sclk_divide_half));

          i_start <= '1';

          WaitForClock(i_clk, 1);
          i_start <= '0';

          WaitForClock(i_clk, 1);
          check_equal(o_ready, '0', result("for o_ready"), level => level);

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("04_n_bits") then

        for bits in 1 to c_config.max_n_bits loop

          n_bits <= bits;
          WaitForClock(i_clk, 1);
          info("n_bits = " & to_string(n_bits));

          i_start <= '1';

          WaitForClock(i_clk, 1);
          i_start <= '0';

          WaitForClock(i_clk, 1);
          check_equal(o_ready, '0', result("for o_ready"), level => level);

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("05_transmit") then

        for k in 1 to 20 loop

          i_d_to_peripheral <= rv.RandSlv(i_d_to_peripheral'length);
          WaitForClock(i_clk, 1);

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("06_receive") then
        info("deterministic");

        for k in 1 to 20 loop

          -- d_from_peripheral_expected <= (others => '1');
          WaitForClock(i_clk, 1);

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

        info("random");

        for k in 1 to 20 loop

          -- d_from_peripheral_expected <= rv.RandSlv(d_from_peripheral_expected'length);
          WaitForClock(i_clk, 1);

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("07_max_n_clks_scs_to_sclk") then

        for clks_scs_to_sclk in 1 to c_config.max_n_clks_scs_to_sclk loop

          n_clks_scs_to_sclk <= clks_scs_to_sclk;
          WaitForClock(i_clk, 1);
          info("n_clks_scs_to_sclk = " & to_string(n_clks_scs_to_sclk));

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("08_max_n_clks_sclk_to_scs") then

        for clks_sclk_to_scs in 1 to c_config.max_n_clks_sclk_to_scs loop

          n_clks_sclk_to_scs <= clks_sclk_to_scs;
          WaitForClock(i_clk, 1);
          info("n_clks_sclk_to_scs = " & to_string(n_clks_sclk_to_scs));

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("09_max_n_clks_rx_sample_strobes_delay") then
        info("deterministic");

        for clks_rx_sample_strobes_delay in 0 to 0 loop -- c_config.max_n_clks_rx_sample_strobes_delay loop

          n_clks_rx_sample_strobes_delay <= clks_rx_sample_strobes_delay;
          -- d_from_peripheral_expected     <= (others => '1');

          WaitForClock(i_clk, 1);
          i_start <= '1';

          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

        info("random");

        for clks_rx_sample_strobes_delay in 0 to c_config.max_n_clks_rx_sample_strobes_delay loop

          -- d_from_peripheral_expected <= rv.RandSlv(d_from_peripheral_expected'length);
          WaitForClock(i_clk, 1);

          i_start <= '1';
          WaitForClock(i_clk, 1);
          i_start <= '0';

          wait until o_ready = '1';

          WaitForClock(i_clk, 5);

        end loop;

      elsif run("10_streaming") then
        transmit_on_sclk_leading_edge <= '1';
        sclk_divide_half              <= 2;
        streaming_mode                <= '1';
        i_keep_streaming              <= '1';

        i_d_to_peripheral <= rv.RandSlv(i_d_to_peripheral'length);
        -- d_from_peripheral_expected <= rv.RandSlv(d_from_peripheral_expected'length);

        n_trx_loops <= 4;

        WaitForClock(i_clk, 1);
        info("n_trx_loops = " & to_string(n_trx_loops));
        info("i_d_to_peripheral ==    " & to_string(i_d_to_peripheral));

        i_start <= '1';
        WaitForClock(i_clk, 1);
        i_start <= '0';

        for k in 1 to n_trx_loops - 2 loop

          wait until rising_edge(o_d_to_peripheral_read_strobe);
          i_d_to_peripheral_expected <= i_d_to_peripheral;
          i_d_to_peripheral          <= rv.RandSlv(i_d_to_peripheral'length);
          WaitForClock(i_clk, 1);
          info("i_d_to_peripheral ==    " & to_string(i_d_to_peripheral));
          -- d_from_peripheral_expected <= rv.RandSlv(d_from_peripheral_expected'length);

          if (k = n_trx_loops - 2) then
            i_keep_streaming <= '0';
          end if;

        end loop;

        wait until rising_edge(o_d_to_peripheral_read_strobe);
        i_d_to_peripheral_expected <= i_d_to_peripheral;
        i_d_to_peripheral          <= rv.RandSlv(i_d_to_peripheral'length);
        WaitForClock(i_clk, 1);
        info("i_d_to_peripheral ==    " & to_string(i_d_to_peripheral));
        -- d_from_peripheral_expected <= rv.RandSlv(d_from_peripheral_expected'length);
        -- i_keep_streaming <= '0';

        WaitForClock(i_clk, 30);
      end if;

    end loop;

    WaitForClock(i_clk, 10);

    test_runner_cleanup(runner);
    wait;

  end process main;

  test_runner_watchdog(runner, 20 us);

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
    WaitForClock(i_clk, 1 + n_clks_scs_to_sclk + (2 * n_bits - 1) * sclk_divide_half);

    -- wait until o_scs and o_le are o_ready
    n_wait := maximum(n_clks_sclk_to_scs, n_clks_sclk_to_le + n_clks_le_width);        -- either o_scs inactive or o_le takes longer

    -- wait until the last sample has been sampled
    if (transmit_on_sclk_leading_edge = '1') then                                      -- receive on o_sclk away from idle state
      n_wait_sample_sdi := n_clks_rx_sample_strobes_delay - sclk_divide_half;
    else
      n_wait_sample_sdi := n_clks_rx_sample_strobes_delay;
    end if;

    n_wait := maximum(n_wait, n_wait_sample_sdi + 2);                                  -- +1 for sampling +1 for o_ready

    WaitForClock(i_clk, n_wait);
    ready_reference <= '1';

    WaitForClock(i_clk, 1);
    ready_reference <= '0';

  end process p_generate_ready_reference;

  p_check_ready : process is
  begin

    wait on o_ready, ready_reference;
    wait for 0 fs;

    if (streaming_mode = '0') then
      check_equal(o_ready, ready_reference, "o_ready is not equal to ready_reference.", level => level);
    end if;

  end process p_check_ready;

  ---------------------------------------------------------------------------
  -- check o_sclk
  ---------------------------------------------------------------------------

  p_generate_sclk_reference : process is
  begin

    wait until rising_edge(i_start);
    sclk_reference <= sclk_idle_state;

    wait until falling_edge(i_start);
    WaitForClock(i_clk, 1);
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

    while not rising_edge(o_ready) loop

      wait on o_ready, o_sclk, sclk_reference;
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

      wait on o_scs, scs_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_scs, scs_reference, "o_scs is not equal to scs_reference.", level => level);
      end if;

    end loop;

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

      wait on o_le, le_reference;
      wait for 0 fs;

      if (streaming_mode = '0') then
        check_equal(o_le, le_reference, "o_le is not equal to le_reference.", level => level);
      end if;

    end loop;

  end process p_check_le;

  ---------------------------------------------------------------------------
  -- check data
  ---------------------------------------------------------------------------

  p_check_data_to_peripheral : process is
  begin

    wait until i_start = '1';

    for k in 1 to n_trx_loops loop

      info("  -> i_d_to_peripheral = " & to_string(i_d_to_peripheral));

      for n in i_d_to_peripheral'left downto i_d_to_peripheral'left - n_bits + 1 loop

        if (transmit_on_sclk_leading_edge = '1') then
          wait_until_sclk_leading_edge(o_sclk, sclk_idle_state);
          check_equal(o_sd_to_peripheral, i_d_to_peripheral(n), result("for o_sd_to_peripheral (n = " & to_string(n) & ")"), level => level);
        else
          wait_until_sclk_trailing_edge(o_sclk, sclk_idle_state);
          check_equal(o_sd_to_peripheral, i_d_to_peripheral(n), result("for o_sd_to_peripheral (n = " & to_string(n) & ")"), level => level);
        end if;

      end loop;

    end loop;

  end process p_check_data_to_peripheral;

  p_generate_sd_from_peripheral : process is
  begin

    wait until rising_edge(i_start);

    d_from_peripheral_expected                      <= (others => '0');
    d_from_peripheral_expected(n_bits - 1 downto 0) <= rv.RandSlv(n_bits);

    i_sd_from_peripheral <= '0';

    wait on o_sclk;

    WaitForClock(i_clk, n_clks_rx_sample_strobes_delay);

    for k in 1 to n_trx_loops loop

      info("  <- d_from_peripheral_expected " & to_string(d_from_peripheral_expected));

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

  -- This is how a module receiving from `spi_master` is supposed to use the two out ports.
  p_check_data_from_peripheral : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      check_o_d_from_peripheral <= '0';

      if (o_d_from_peripheral_read_strobe) then
        check_equal(o_d_from_peripheral, d_from_peripheral_expected_old, result("for o_d_from_peripheral"), level => level);
        check_o_d_from_peripheral <= '1';
      end if;
    end if;

  end process p_check_data_from_peripheral;

  ---------------------------------------------------------------------------
  -- Check if enough checks have been made.
  ---------------------------------------------------------------------------

  p_count_checks : process is
  begin

    wait until i_start = '1';
    n_checks_d_from_peripheral <= 0;

    while not o_ready loop

      wait on check_o_d_from_peripheral, o_ready;

      if (check_o_d_from_peripheral) then
        n_checks_d_from_peripheral <= n_checks_d_from_peripheral + 1;
      end if;

    end loop;

  end process p_count_checks;

  p_check_n_checks : process is
  begin

    wait until o_ready = '1';
    check_equal(n_checks_d_from_peripheral, n_trx_loops, level => level);

  end process p_check_n_checks;

end architecture arch;

