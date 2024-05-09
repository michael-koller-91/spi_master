library ieee;
  use ieee.math_real.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_1164.all;

package spi_package is

  ---------------------------------------------------------------------------
  -- Description of generics and ports to configure the SPI master.
  ---------------------------------------------------------------------------

  type t_config is record
    -- The maximum number of system clock cycles from SCS active to the first SCLK edge.
    max_n_clks_scs_to_sclk : positive;

    -- The maximum number of system clock cycles from the last SCLK edge to SCS inactive.
    max_n_clks_sclk_to_scs : positive;

    -- The maximum number of bits to receive/transmit.
    max_n_bits : positive;

    -- The maximum number of system clock cycles for half an SCLK cycle.
    -- Example 1:
    --    With G_MAX_SCLK_DIVIDE_HALF = 1, the system clock (i_clk)
    --    can be divided by 1 * 2. A 10 MHz system clock will lead to
    --    a 5 MHz SCLK.
    -- Example 2:
    --    With G_MAX_SCLK_DIVIDE_HALF = 2, the system clock (i_clk)
    --    can be divided by 1 * 2 or 2 * 2. A 10 MHz system clock will lead to
    --    a 5 MHz or a 2.5 MHz SCLK.
    max_sclk_divide_half : positive;

    -- The maximum number of system clock cycles from the last SCLK edge to the latch enable signal.
    max_n_clks_sclk_to_le : positive;

    -- The maximum number of system clock cycles which the latch enable signal is high.
    max_n_clks_le_width : positive;

    -- The maximum number of system clock cycles by which the receive sample strobes can be delayed.
    max_n_clks_rx_sample_strobes_delay : positive;
  end record t_config;

  constant c_default_config : t_config :=
  (
    max_n_clks_scs_to_sclk             => 1,
    max_n_clks_sclk_to_scs             => 1,
    max_n_bits                         => 1,
    max_sclk_divide_half               => 1,
    max_n_clks_sclk_to_le              => 1,
    max_n_clks_le_width                => 1,
    max_n_clks_rx_sample_strobes_delay => 0
  );

  type t_settings is record
    -- The idle value of SCS.
    scs_idle_state : std_ulogic;

    -- The idle value of SCLK.
    sclk_idle_state : std_ulogic;

    -- The SCLK has two edges: The first/leading/left edge and the second/trailing/right edge.
    -- If `transmit_on_sclk_leading_edge` is equal to '1', data is transmitted to the peripheral on the leading edge.
    -- If `transmit_on_sclk_leading_edge` is equal to '0', data is transmitted to the peripheral on the trailing edge.
    transmit_on_sclk_leading_edge : std_ulogic;

    -- The strobes used to sample data received from the peripheral can be
    -- delayed by this number of system clock cycles.
    n_clks_rx_sample_strobes_delay : unsigned;

    -- If `N` system clock cycles should make up one SCLK cycle,
    -- set this variable to `N / 2 - 1`.
    sclk_divide_half_minus_1 : unsigned;

    -- If `N` bits should be transmitted and received,
    -- set this variable to `N - 1`.
    n_bits_minus_1 : unsigned;

    -- If `N` system clock cycles should pass from SCS going active to the first SCLK edge,
    -- set this variable to `N - 1`.
    n_clks_scs_to_sclk_minus_1 : unsigned;

    -- If `N` system clock cycles should pass from the last SCLK edge to SCS going inactive,
    -- set this variable to `N - 1`.
    n_clks_sclk_to_scs_minus_1 : unsigned;

    -- If `N` system clock cycles should pass from the last SCLK edge to LE going high,
    -- set this variable to `N - 1`.
    n_clks_sclk_to_le_minus_1 : unsigned;

    -- If LE should stay high for `N` system clock cycles,
    -- set this variable to `N - 1`.
    n_clks_le_width_minus_1 : unsigned;
  end record t_settings;

  ---------------------------------------------------------------------------
  -- This part is only relevant for the implementation.
  ---------------------------------------------------------------------------

  -- States for state machines.

  type t_state is (idle, wait_sclk, trx, wait_scs_and_le_and_sample_sdi);

  type t_sclk_fsm_state is (inactive, active);

  type t_scs_fsm_state is (inactive, active);

  type t_le_fsm_state is (idle, wait_until_sclk_done, wait_until_active, active);

  type t_streaming_state is (idle, detect_streaming, streaming);

  -- How many bits are needed to represent `value` values?

  function ceil_log2 (
    value : in positive
  )
    return positive;

end package spi_package;

package body spi_package is

  function ceil_log2 (
    value : in positive
  )
    return positive is
  begin

    return positive(realmax(ceil(log2(real(value))), 1.0));

  end function;

end package body spi_package;

