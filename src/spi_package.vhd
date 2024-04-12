library ieee;
use ieee.math_real.all;

package spi_package is

  type t_config is record
    -- The maximum number of clock cycles from SCS active to the first SCLK edge.
    max_n_clks_scs_to_sclk : positive;

    -- The maximum number of clock cycles from the last SCLK edge to SCS inactive.
    max_n_clks_sclk_to_scs : positive;

    -- The maximum number of bits to receive/transmit.
    max_n_bits : positive;

    -- The maximum number of clock cycles for half an SCLK cycle.
    -- Example 1:
    --    With G_MAX_SCLK_DIVIDE_HALF = 1, the system clock (i_clk)
    --    can be divided by 1 * 2. A 10 MHz system clock will lead to
    --    a 5 MHz SCLK.
    -- Example 2:
    --    With G_MAX_SCLK_DIVIDE_HALF = 2, the system clock (i_clk)
    --    can be divided by 1 * 2 or 2 * 2. A 10 MHz system clock will lead to
    --    a 5 MHz or a 2.5 MHz SCLK.
    max_sclk_divide_half : positive;

    -- The maximum number of clock cycles from the last SCLK edge to the latch enable signal.
    max_n_clks_sclk_to_le : positive;

    -- The maximum number of clock cycles which the latch enable signal is high.
    max_n_clks_le_width : positive;

    -- The maximum number of clock cycles by which the receive sample strobes can be delayed.
    max_n_clks_rx_sample_strobes_delay : natural;
  end record t_config;

  constant C_DEFAULT_CONFIG : t_config := (
    max_n_clks_scs_to_sclk             => 1,
    max_n_clks_sclk_to_scs             => 1,
    max_n_bits                         => 1,
    max_sclk_divide_half               => 1,
    max_n_clks_sclk_to_le              => 1,
    max_n_clks_le_width                => 1,
    max_n_clks_rx_sample_strobes_delay => 0
    );

  -- How many bits are needed to represent `value` values?
  function ceil_log2(value : in positive)
    return positive;

end package spi_package;

package body spi_package is

  function ceil_log2(value : in positive)
    return positive is
  begin
    return positive(realmax(ceil(log2(real(value))), 1.0));
  end;

end package body spi_package;
