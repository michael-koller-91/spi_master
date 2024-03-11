library ieee;
use ieee.std_logic_1164.all;

library vunit_lib;
context vunit_lib.vunit_context;

library osvvm;
use osvvm.TbUtilPkg.all;

entity tb_spi_master is
  generic (
    runner_cfg                                : string;
    G_SCLK_IDLE_STATE                         : std_ulogic := '1';
    G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE : boolean    := true;
    G_SCS_IDLE_STATE                          : std_ulogic := '1';
    G_N_CLKS_SCS_TO_SCLK                      : positive    := 3;
    G_N_CLKS_SCLK_TO_SCS                      : positive    := 4;
    G_N_BITS                                  : positive    := 3;
    G_CLK_DIVIDE                              : positive    := 4
    );
end entity;

architecture arch of tb_spi_master is

  procedure wait_until_scs_active(signal scs : std_ulogic) is
  begin
    if G_SCS_IDLE_STATE = '0' then
      wait until rising_edge(scs);
    else
      wait until falling_edge(scs);
    end if;
  end procedure;

  procedure wait_until_scs_inactive(signal scs : std_ulogic) is
  begin
    if G_SCS_IDLE_STATE = '0' then
      wait until falling_edge(scs);
    else
      wait until rising_edge(scs);
    end if;
  end procedure;

  procedure wait_until_sclk_edge_away_from_idle(signal sclk : std_ulogic) is
  begin
    if G_SCLK_IDLE_STATE = '0' then
      wait until rising_edge(sclk);
    else
      wait until falling_edge(sclk);
    end if;
  end procedure;

  procedure wait_until_sclk_edge_toward_idle(signal sclk : std_ulogic) is
  begin
    if G_SCLK_IDLE_STATE = '0' then
      wait until falling_edge(sclk);
    else
      wait until rising_edge(sclk);
    end if;
  end procedure;

  constant C_CLK_PERIOD : time := 10 ns;

  signal clk                : std_ulogic                               := '0';
  signal d_from_peripheral  : std_ulogic_vector(G_N_BITS - 1 downto 0) := (others => '0');
  signal d_to_peripheral    : std_ulogic_vector(G_N_BITS - 1 downto 0) := (others => '0');
  signal start              : std_ulogic                               := '0';
  signal ready              : std_ulogic                               := '0';
  signal sclk               : std_ulogic                               := '0';
  signal sd_from_peripheral : std_ulogic                               := '0';
  signal sd_to_peripheral   : std_ulogic                               := '0';
  signal scs                : std_ulogic                               := '0';

begin

  CreateClock(clk, C_CLK_PERIOD);

  main : process
  begin
    test_runner_setup(runner, runner_cfg);

    WaitForClock(clk, 10);

    while test_suite loop
      if run("test_001") then
        start <= '1';
        WaitForClock(clk, 1);
        start <= '0';

        WaitForClock(clk, 1);
        check_equal(ready, '0', result("for ready"));

        wait until rising_edge(ready);
        WaitForClock(clk, 2);
        check_equal(ready, '0', result("for ready"));

        WaitForClock(clk, 5);
      end if;
    end loop;

    WaitForClock(clk, 10);

    test_runner_cleanup(runner);
    wait;

  end process;

  test_runner_watchdog(runner, 2 us);

  p_check_time_sclk_and_scs : process
    variable tic1 : time := 0 ns;
    variable tic2 : time := 0 ns;
    variable tic3 : time := 0 ns;
    variable tic4 : time := 0 ns;
    variable toc1 : time := 0 ns;
    variable toc2 : time := 0 ns;
    variable toc3 : time := 0 ns;
    variable toc4 : time := 0 ns;
  begin
    wait on start;

    wait_until_scs_active(scs);
    tic1 := now;

    wait_until_sclk_edge_away_from_idle(sclk);
    toc1 := now;
    check_equal((toc1 - tic1) / C_CLK_PERIOD, G_N_CLKS_SCS_TO_SCLK, "The time difference between SCS active and the first SCLK edge is not correct.");

    tic2 := now;

    tic3 := now;
    for k in 1 to 2 * (G_N_BITS - 1) loop
      info("k = " & to_string(k));
      wait on sclk;
      toc3 := now;
      check_equal((toc3 - tic3) / C_CLK_PERIOD, G_CLK_DIVIDE / 2, "The time difference between two consecutive SCLK edges is not correct.");
      tic3 := now;
    end loop;

    wait_until_sclk_edge_toward_idle(sclk);
    toc2 := now;
    check_equal((toc2 - tic2) / C_CLK_PERIOD, G_N_BITS * G_CLK_DIVIDE - G_CLK_DIVIDE / 2, "The time difference between the first and the last SCLK edges is not correct.");
    tic4 := now;

    wait_until_scs_inactive(scs);
    toc4 := now;
    check_equal((toc4 - tic4) / C_CLK_PERIOD, G_N_CLKS_SCLK_TO_SCS, "The time difference between the last sclk edge and SCS inactive is not correct.");

    check_equal((toc4 - tic1) / C_CLK_PERIOD, G_N_CLKS_SCS_TO_SCLK + G_N_BITS * G_CLK_DIVIDE - G_CLK_DIVIDE / 2 + G_N_CLKS_SCLK_TO_SCS, "The SCS active time is not correct.");
  end process;

  dut_i : entity work.spi_master(arch)
    generic map(
      G_SCLK_IDLE_STATE                         => G_SCLK_IDLE_STATE,
      G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE => G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE,
      G_SCS_IDLE_STATE                          => G_SCS_IDLE_STATE,
      G_N_CLKS_SCS_TO_SCLK                      => G_N_CLKS_SCS_TO_SCLK,
      G_N_CLKS_SCLK_TO_SCS                      => G_N_CLKS_SCLK_TO_SCS,
      G_N_BITS                                  => G_N_BITS,
      G_CLK_DIVIDE                              => G_CLK_DIVIDE
      )
    port map(
      i_clk                => clk,
      i_start              => start,
      o_ready              => ready,
      o_d_from_peripheral  => d_from_peripheral,
      i_d_to_peripheral    => d_to_peripheral,
      o_sclk               => sclk,
      i_sd_from_peripheral => sd_from_peripheral,
      o_sd_to_peripheral   => sd_to_peripheral,
      o_scs                => scs
      );

end architecture;

