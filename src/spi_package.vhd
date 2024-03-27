library ieee;
use ieee.math_real.all;

package spi_package is

  -- how many bits are needed to represent `value` values
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
