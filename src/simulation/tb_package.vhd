library vunit_lib;
  context vunit_lib.vunit_context;

package tb_package is

  type t_check_counter is protected

    procedure print_values;

    procedure reset;

    procedure inc_le;

    procedure inc_ready;

    procedure inc_scs;

    procedure inc_sclk;

    procedure inc_sd_to_peripheral;

    procedure inc_sd_from_peripheral;

    procedure all_equal_to (
      n_checks_expected : positive
    );

  end protected;

end package tb_package;

package body tb_package is

  type t_check_counter is protected body

    variable n_le                 : natural;
    variable n_ready              : natural;
    variable n_scs                : natural;
    variable n_sclk               : natural;
    variable n_sd_to_peripheral   : natural;
    variable n_sd_from_peripheral : natural;

    procedure print_values is
    begin

      info("n_le                 = " & to_string(n_le));
      info("n_ready              = " & to_string(n_ready));
      info("n_scs                = " & to_string(n_scs));
      info("n_sclk               = " & to_string(n_sclk));
      info("n_sd_to_peripheral   = " & to_string(n_sd_to_peripheral));
      info("n_sd_from_peripheral = " & to_string(n_sd_from_peripheral));

    end procedure;

    procedure reset is
    begin

      n_le                 := 0;
      n_ready              := 0;
      n_scs                := 0;
      n_sclk               := 0;
      n_sd_to_peripheral   := 0;
      n_sd_from_peripheral := 0;

    end procedure;

    procedure inc_le is
    begin

      n_le := n_le + 1;

    end procedure;

    procedure inc_ready is
    begin

      n_ready := n_ready + 1;

    end procedure;

    procedure inc_scs is
    begin

      n_scs := n_scs + 1;

    end procedure;

    procedure inc_sclk is
    begin

      n_sclk := n_sclk + 1;

    end procedure;

    procedure inc_sd_to_peripheral is
    begin

      n_sd_to_peripheral := n_sd_to_peripheral + 1;

    end procedure;

    procedure inc_sd_from_peripheral is
    begin

      n_sd_from_peripheral := n_sd_from_peripheral + 1;

    end procedure;

    procedure all_equal_to (
      n_checks_expected : positive
    ) is
    begin

      check_equal(n_le, n_checks_expected, result("for the number of le checks"));
      check_equal(n_ready, n_checks_expected, result("for the number of ready checks"));
      check_equal(n_scs, n_checks_expected, result("for the number of scs checks"));
      check_equal(n_sclk, n_checks_expected, result("for the number of sclk checks"));
      check_equal(n_sd_to_peripheral, n_checks_expected, result("for the number of sd_to_peripheral checks"));
      check_equal(n_sd_from_peripheral, n_checks_expected, result("for the number of sd_from_peripheral checks"));

    end procedure;

  end protected body;

end package body tb_package;

