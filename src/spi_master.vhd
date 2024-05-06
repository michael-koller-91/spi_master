library ieee;
  use ieee.math_real.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_1164.all;

library spi_lib;
  use spi_lib.spi_package.all;

entity spi_master is
  generic (
    g_config : t_config := c_default_config
  );
  port (
    i_clk : in    std_ulogic := '0'; -- system clock
    -- control
    i_start           : in    std_ulogic := '0';
    o_busy            : out   std_ulogic := '0';
    o_ready           : out   std_ulogic := '0';
    i_keep_streaming  : in    std_ulogic := '0';
    o_streaming_start : out   std_ulogic := '0';
    -- data
    i_d_to_peripheral               : in    std_ulogic_vector(g_config.max_n_bits - 1 downto 0) := (others => '0');
    o_d_from_peripheral             : out   std_ulogic_vector(g_config.max_n_bits - 1 downto 0) := (others => '0');
    o_d_from_peripheral_read_strobe : out   std_ulogic                                          := '0'; -- use this to sample o_d_from_peripheral
    -- settings
    i_settings : in    t_settings(
      sclk_divide_half_minus_1 (ceil_log2(g_config.max_sclk_divide_half) - 1 downto 0),
      n_bits_minus_1 (ceil_log2(g_config.max_n_bits) - 1 downto 0),
      n_clks_scs_to_sclk_minus_1 (ceil_log2(g_config.max_n_clks_scs_to_sclk) - 1 downto 0),
      n_clks_sclk_to_scs_minus_1 (ceil_log2(g_config.max_n_clks_sclk_to_scs) - 1 downto 0),
      n_clks_sclk_to_le_minus_1 (ceil_log2(g_config.max_n_clks_sclk_to_le) - 1 downto 0),
      n_clks_le_width_minus_1 (ceil_log2(g_config.max_n_clks_le_width) - 1 downto 0),
      n_clks_rx_sample_strobes_delay (ceil_log2(g_config.max_n_clks_rx_sample_strobes_delay + 1) - 1 downto 0)
      );
    -- SPI signals
    o_le                 : out   std_ulogic := '0';
    o_scs                : out   std_ulogic := '1';
    o_sclk               : out   std_ulogic := '1';
    o_sd_to_peripheral   : out   std_ulogic := '0';
    i_sd_from_peripheral : in    std_ulogic := '0'
  );
end entity spi_master;

architecture arch of spi_master is

  signal counter_clk_divide         : natural range 0 to g_config.max_sclk_divide_half - 1   := 0;
  signal counter_n_sclk_edges       : natural range 0 to 2 * g_config.max_n_bits - 1         := 2 * g_config.max_n_bits - 1;
  signal counter_n_sclk_edges2g     : natural range 0 to 2 * g_config.max_n_bits - 1         := 2 * g_config.max_n_bits - 1;
  signal counter_n_clks_sclk_to_scs : natural range 0 to g_config.max_n_clks_sclk_to_scs - 1 := g_config.max_n_clks_sclk_to_scs - 1;
  -- signal counter_n_clks_scs_to_sclk : natural range 0 to g_config.max_n_clks_scs_to_sclk - 1 := g_config.max_n_clks_scs_to_sclk - 1;
  signal counter_n_clks_sclk_to_le : natural range 0 to g_config.max_n_clks_sclk_to_le - 1 := g_config.max_n_clks_sclk_to_le - 1;
  signal counter_n_clks_le_width   : natural range 0 to g_config.max_n_clks_le_width - 1   := g_config.max_n_clks_le_width - 1;
  signal counter_n_sample_sdi      : natural range 0 to g_config.max_n_bits                := g_config.max_n_bits;

  signal sclk_internal      : std_ulogic := '0';
  signal sclk_internal_reg1 : std_ulogic := '1';
  signal sclk_internal_reg2 : std_ulogic := '1';
  signal sclk_stop          : std_ulogic := '0';
  signal scs                : std_ulogic := '1';

  signal sclk_internal_edge             : std_ulogic                                                              := '0';
  signal sclk_internal_leading_edge     : std_ulogic                                                              := '0';
  signal sample_sdo_sreg                : std_ulogic_vector(g_config.max_sclk_divide_half - 1 downto 0)           := (others => '0');
  signal sclk_internal_leading_edge_reg : std_ulogic                                                              := '0';
  signal sample_sdi_sreg                : std_ulogic_vector(g_config.max_n_clks_rx_sample_strobes_delay downto 0) := (others => '0');
  signal sample_sdi                     : std_ulogic                                                              := '0';
  signal sample_sdi_reg                 : std_ulogic                                                              := '0';
  signal sample_sdo                     : std_ulogic                                                              := '0';

  signal state     : t_state         := idle;
  signal le_state  : t_le_fsm_state  := idle;
  signal scs_state : t_scs_fsm_state := inactive;
  signal sclk_done : std_ulogic      := '0';

  signal le        : std_ulogic := '0';
  signal enable_le : std_ulogic := '0';

  signal busy               : std_ulogic                                                   := '0';
  signal ready              : std_ulogic                                                   := '0';
  signal sclk_start         : std_ulogic                                                   := '1';
  signal reset_sclk         : std_ulogic                                                   := '1';
  signal reset_sclk_delayed : std_ulogic                                                   := '1';
  signal reset_sclk_sreg    : std_ulogic_vector(g_config.max_n_clks_sclk_to_scs  downto 1) := (others => '0');

  signal streaming_start : std_ulogic := '0';

  -- sampled control
  signal start          : std_ulogic := '0';
  signal keep_streaming : std_ulogic := '0';

  -- sampled settings
  signal streaming_mode                 : std_ulogic                                                     := '0';
  signal sclk_divide_half_minus_1_2g    : natural range 0 to g_config.max_sclk_divide_half - 1           := 0;
  signal transmit_on_sclk_leading_edge  : std_ulogic                                                     := '1';
  signal n_clks_sclk_to_scs_minus_1     : natural range 0 to g_config.max_n_clks_sclk_to_scs - 1         := 0;
  signal n_clks_sclk_to_le_minus_1      : natural range 0 to g_config.max_n_clks_sclk_to_le - 1          := 0;
  signal n_clks_le_width_minus_1        : natural range 0 to g_config.max_n_clks_le_width - 1            := 0;
  signal n_clks_rx_sample_strobes_delay : natural range 0 to g_config.max_n_clks_rx_sample_strobes_delay := 0;
  signal n_clks_scs_to_sclk_minus_1     : natural range 0 to g_config.max_n_clks_scs_to_sclk - 1         := 0;

  -- data
  signal d_to_peripheral        : std_ulogic_vector(g_config.max_n_bits - 1 downto 0) := (others => '0');
  signal d_from_peripheral      : std_ulogic_vector(g_config.max_n_bits - 1 downto 0) := (others => '0');
  signal sd_from_peripheral_reg : std_ulogic                                          := '0';
  signal sdo_reg                : std_ulogic                                          := '0';

begin

  ---------------------------------------------------------------------------
  -- handle in-ports
  ---------------------------------------------------------------------------

  -- block start if busy
  start <= i_start and not busy;

  p_sample_settings : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (start = '1') then
        streaming_mode                 <= i_settings.streaming_mode;
        transmit_on_sclk_leading_edge  <= i_settings.transmit_on_sclk_leading_edge;
        sclk_divide_half_minus_1_2g    <= to_integer(i_settings.sclk_divide_half_minus_1);
        n_clks_sclk_to_scs_minus_1     <= to_integer(i_settings.n_clks_sclk_to_scs_minus_1);
        n_clks_sclk_to_le_minus_1      <= to_integer(i_settings.n_clks_sclk_to_le_minus_1);
        n_clks_le_width_minus_1        <= to_integer(i_settings.n_clks_le_width_minus_1);
        n_clks_rx_sample_strobes_delay <= to_integer(i_settings.n_clks_rx_sample_strobes_delay);
        n_clks_scs_to_sclk_minus_1     <= to_integer(i_settings.n_clks_scs_to_sclk_minus_1);
      end if;
    end if;

  end process p_sample_settings;

  p_input_register : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      sd_from_peripheral_reg <= i_sd_from_peripheral;
    end if;

  end process p_input_register;

  keep_streaming <= i_keep_streaming;

  ---------------------------------------------------------------------------
  -- handle out-ports
  ---------------------------------------------------------------------------

  o_streaming_start <= streaming_start;

  o_d_from_peripheral <= d_from_peripheral;

  o_d_from_peripheral_read_strobe <= sample_sdi_reg when counter_n_sample_sdi = 0 else
                                     '0';

  o_busy  <= busy;
  o_ready <= ready;

  o_sclk <= sclk_internal_reg2;

  p_output_register : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      o_scs <= scs;
      o_le  <= le;
    end if;

  end process p_output_register;

  o_sd_to_peripheral <= sdo_reg;

  ---------------------------------------------------------------------------
  -- The control FSM.
  ---------------------------------------------------------------------------

  p_fsm : process (i_clk) is
  -- variable counter_n_clks_scs_to_sclk : integer range 0 to g_config.max_n_clks_scs_to_sclk - 1 := 0;
  begin

    if rising_edge(i_clk) then

      case state is

        when wait_sclk =>

          -- if (counter_n_clks_scs_to_sclk = 1) then
          -- if (counter_n_clks_scs_to_sclk = 0) then
          --  reset_sclk <= '0';
          --  state      <= trx;
          -- else
          --  -- counter_n_clks_scs_to_sclk <= counter_n_clks_scs_to_sclk - 1;
          --  counter_n_clks_scs_to_sclk := counter_n_clks_scs_to_sclk - 1;
          -- end if;
          state <= trx;

        when trx =>

          streaming_start <= '0';
          if (counter_n_sclk_edges2g = 0) then
            if (counter_clk_divide = 0) then
              streaming_start        <= '1';
              counter_n_sclk_edges2g <= to_integer(i_settings.n_bits_minus_1 & '1');
              if not(streaming_mode = '1' and keep_streaming = '1') then
                reset_sclk <= '1';
                sclk_stop  <= '1';
                state      <= wait_scs_and_le_and_sample_sdi;
              end if;
            -- counter_n_clks_sclk_to_scs <= n_clks_sclk_to_scs_minus_1;
            -- counter_n_clks_sclk_to_le <= n_clks_sclk_to_le_minus_1;
            -- counter_n_clks_le_width <= n_clks_le_width_minus_1;
            end if;
          else
            if (counter_clk_divide = 0) then
              counter_n_sclk_edges2g <= counter_n_sclk_edges2g - 1;
            end if;
          end if;

        when wait_scs_and_le_and_sample_sdi =>

          sclk_stop <= '0';

          -- if (counter_n_clks_sclk_to_scs = 0) then
          ---- scs <= i_settings.scs_idle_state;
          -- else
          --  counter_n_clks_sclk_to_scs <= counter_n_clks_sclk_to_scs - 1;
          -- end if;

          if (enable_le = '1') then
            if (counter_n_clks_sclk_to_le = 0) then
              -- le        <= '1';
              enable_le <= '0';
            else
            -- counter_n_clks_sclk_to_le <= counter_n_clks_sclk_to_le - 1;
            end if;
          end if;

          if (le = '1') then
            if (counter_n_clks_le_width = 0) then
            -- le <= '0';
            else
            -- counter_n_clks_le_width <= counter_n_clks_le_width - 1;
            end if;
          end if;

          if (counter_n_clks_sclk_to_scs = 0 and counter_n_clks_le_width = 0 and counter_n_sample_sdi = 0) then
            ready <= '1';
            state <= idle;
          end if;

        when others =>                                                                                          -- idle

          busy  <= '0';
          ready <= '0';

          -- make in-port change visible at out-port without the need of a start strobe
          -- scs <= i_settings.scs_idle_state;

          if (start) then
            -- `counter_n_sclk_edges2g` counts 2 * n_bits sclk edges:
            --    from 2 * n_bits - 1 downto 0
            --    <=> from 2 * (n_bits - 1) + 1 downto 0
            counter_n_sclk_edges2g <= to_integer(i_settings.n_bits_minus_1 & '1');
            --
            -- counter_n_clks_scs_to_sclk <= to_integer(i_settings.n_clks_scs_to_sclk_minus_1);
            -- counter_n_clks_scs_to_sclk := to_integer(i_settings.n_clks_scs_to_sclk_minus_1);
            -- scs <= not i_settings.scs_idle_state;
            --
            enable_le <= '1';
            --
            busy <= '1';

            reset_sclk <= '0';

            -- if (i_settings.n_clks_scs_to_sclk_minus_1 = 0) then
            -- if (counter_n_clks_scs_to_sclk = 0) then
            --  -- reset_sclk <= '0';
            --  state <= trx;
            -- else
            -- counter_n_clks_scs_to_sclk := counter_n_clks_scs_to_sclk - 1;
            state <= wait_sclk;
          -- end if;
          end if;

      end case;

    end if;

  end process p_fsm;

  p_count_sample_sdi_strobes : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (start = '1') then -- or streaming_start = '1') then
        counter_n_sample_sdi <= to_integer(i_settings.n_bits_minus_1) + 1;
      else
        if (sample_sdi = '1') then
          counter_n_sample_sdi <= counter_n_sample_sdi - 1;
        end if;
      end if;
    end if;

  end process p_count_sample_sdi_strobes;

  ---------------------------------------------------------------------------
  -- Generate LE.
  ---------------------------------------------------------------------------

  p_generate_le : process (i_clk) is
  begin

    if rising_edge(i_clk) then

      case le_state is

        when wait_until_sclk_done =>

          if (sclk_done) then
            le_state <= wait_until_active;
          end if;

        when wait_until_active =>

          if (counter_n_clks_sclk_to_le = 0) then
            le                      <= '1';
            counter_n_clks_le_width <= n_clks_le_width_minus_1;
            le_state                <= active;
          else
            counter_n_clks_sclk_to_le <= counter_n_clks_sclk_to_le - 1;
          end if;

        when active =>

          if (counter_n_clks_le_width = 0) then
            le       <= '0';
            le_state <= idle;
          else
            counter_n_clks_le_width <= counter_n_clks_le_width - 1;
          end if;

        when others =>

          le <= '0';

          if (start) then
            counter_n_clks_sclk_to_le <= to_integer(i_settings.n_clks_sclk_to_le_minus_1);
            le_state                  <= wait_until_sclk_done;
          end if;

      end case;

    end if;

  end process p_generate_le;

  ---------------------------------------------------------------------------
  -- Generate SCS.
  ---------------------------------------------------------------------------

  p_generate_scs : process (i_clk) is
  begin

    if rising_edge(i_clk) then

      case scs_state is

        when active =>

          if (sclk_done) then
            if (counter_n_clks_sclk_to_scs = 0) then
              scs_state <= inactive;
            else
              counter_n_clks_sclk_to_scs <= counter_n_clks_sclk_to_scs - 1;
            end if;
          end if;

        when others =>

          -- make in-port change visible at out-port without the need of a start strobe
          scs <= i_settings.scs_idle_state;

          if (start) then
            scs <= not i_settings.scs_idle_state;

            counter_n_clks_sclk_to_scs <= to_integer(i_settings.n_clks_sclk_to_scs_minus_1);
            scs_state                  <= active;
          end if;

      end case;

    end if;

  end process p_generate_scs;

  ---------------------------------------------------------------------------
  -- Generate SCLK.
  ---------------------------------------------------------------------------

  p_reset_sclk_delayed : process (all) is
  begin

    if (n_clks_scs_to_sclk_minus_1 = 0) then
      sclk_start         <= start;
      reset_sclk_delayed <= reset_sclk;
    else
      reset_sclk_delayed <= reset_sclk_sreg(n_clks_scs_to_sclk_minus_1);
    end if;

  end process p_reset_sclk_delayed;

  generate_reset_sclk_delayed : if g_config.max_n_clks_scs_to_sclk = 1 generate

    p_delay_reset_sclk : process (i_clk) is
    begin

      if rising_edge(i_clk) then
        reset_sclk_sreg(1) <= reset_sclk;
      end if;

    end process p_delay_reset_sclk;

  else generate

    p_delay_reset_sclk : process (i_clk) is
    begin

      if rising_edge(i_clk) then
        reset_sclk_sreg <= reset_sclk_sreg(reset_sclk_sreg'left - 1 downto reset_sclk_sreg'right) & reset_sclk;
      end if;

    end process p_delay_reset_sclk;

  end generate generate_reset_sclk_delayed;

  p_generate_sclk : process (i_clk) is

    variable sclk_state                 : t_sclk_fsm_state                                       := inactive;
    variable start_came                 : boolean                                                := false;
    variable counter_n_clks_scs_to_sclk : integer range 0 to g_config.max_n_clks_scs_to_sclk - 1 := 0;
    variable sclk_divide_half_minus_1   : natural range 0 to g_config.max_sclk_divide_half - 1   := 0;

  begin

    if rising_edge(i_clk) then

      case sclk_state is

        when active =>

          start_came := false;

          sclk_internal_edge <= '0';
          if (counter_clk_divide = 0) then
            counter_clk_divide <= sclk_divide_half_minus_1_2g;
            sclk_internal      <= not sclk_internal;
            sclk_internal_edge <= '1';

            counter_n_sclk_edges <= counter_n_sclk_edges - 1;
            if (counter_n_sclk_edges = 1) then
              sclk_state := inactive;
              sclk_done  <= '1';
            end if;
          else
            counter_clk_divide <= counter_clk_divide - 1;
          end if;

        when others =>

          counter_clk_divide <= 0;
          sclk_internal      <= '0';
          sclk_internal_edge <= '0';

          if (start = '1') then
            start_came                 := true;
            sclk_done                  <= '0';
            counter_n_clks_scs_to_sclk := to_integer(i_settings.n_clks_scs_to_sclk_minus_1);
            sclk_divide_half_minus_1   := to_integer(i_settings.sclk_divide_half_minus_1);
          end if;

          if (start_came) then
            if (counter_n_clks_scs_to_sclk = 0) then
              counter_clk_divide <= sclk_divide_half_minus_1;
              sclk_internal      <= not sclk_internal;
              sclk_internal_edge <= '1';

              -- `counter_n_sclk_edges` counts 2 * n_bits sclk edges:
              --    from 2 * n_bits - 1 downto 0
              --    <=> from 2 * (n_bits - 1) + 1 downto 0
              counter_n_sclk_edges <= to_integer(i_settings.n_bits_minus_1 & '1');

              sclk_state := active;
            else
              counter_n_clks_scs_to_sclk := counter_n_clks_scs_to_sclk - 1;
            end if;
          end if;

      end case;

      -- Two delays in order to be able to make use of the `sclk_internal_edge` signal
      -- for sampling of the incoming and outgoing data.

      if (i_settings.sclk_idle_state = '0') then
        sclk_internal_reg1 <= sclk_internal;
      else
        sclk_internal_reg1 <= not sclk_internal;
      end if;

      sclk_internal_reg2 <= sclk_internal_reg1;
    end if;

  end process p_generate_sclk;

  sclk_internal_leading_edge <= sclk_internal_edge and sclk_internal;

  ---------------------------------------------------------------------------
  -- This part generates `sample_sdo` such that it is high two clock
  -- cycles before the SCLK edge at which `o_sd_to_peripheral`
  -- is supposed to be stable/valid.
  -- As a result the data is already stable/valid one clock cycle
  -- before the SCLK edge.
  ---------------------------------------------------------------------------

  generate_sample_sdo : if g_config.max_sclk_divide_half = 1 generate
    ---------------------------------------------------------------------------
    -- In this case, the "shift register" has a length of 1 and therefore does
    -- not really shift. In particular, `'left - 1 downto 0` doesn't work.
    ---------------------------------------------------------------------------

    p_delay_sample_sdo : process (i_clk) is
    begin

      if rising_edge(i_clk) then
        sample_sdo_sreg(sample_sdo_sreg'left) <= sclk_internal_leading_edge;
      end if;

    end process p_delay_sample_sdo;

    p_sample_sdo : process (all) is
    begin

      if (transmit_on_sclk_leading_edge = '1') then
        sample_sdo <= sclk_internal_leading_edge;
      else
        sample_sdo <= sample_sdo_sreg(sample_sdo_sreg'left);
      end if;

    end process p_sample_sdo;

  else generate

    p_delay_sample_sdo : process (i_clk) is
    begin

      if rising_edge(i_clk) then
        sample_sdo_sreg <= sample_sdo_sreg(sample_sdo_sreg'left - 1 downto 0) & sclk_internal_leading_edge;
      end if;

    end process p_delay_sample_sdo;

    p_sample_sdo : process (all) is
    begin

      if (transmit_on_sclk_leading_edge = '1') then
        sample_sdo <= sclk_internal_leading_edge;
      else
        sample_sdo <= sample_sdo_sreg(sclk_divide_half_minus_1_2g);
      end if;

    end process p_sample_sdo;

  end generate generate_sample_sdo;

  ---------------------------------------------------------------------------
  -- Sample the data going to the peripheral.
  ---------------------------------------------------------------------------

  p_transmit_to_peripheral : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (start) then
        d_to_peripheral <= i_d_to_peripheral;
      else
        if (sample_sdo) then
          d_to_peripheral <= d_to_peripheral(d_to_peripheral'left - 1 downto 0) & '0';
          sdo_reg         <= d_to_peripheral(d_to_peripheral'left);
        end if;
      end if;
    end if;

  end process p_transmit_to_peripheral;

  ---------------------------------------------------------------------------
  -- Delay the sample strobe which is used to sample the data
  -- coming from the peripheral.
  ---------------------------------------------------------------------------

  p_delay_sample_sdi : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      -- This edge is one clock cycle before the edge we want to use to sample sdi.
      sclk_internal_leading_edge_reg <= sclk_internal_leading_edge;

      -- So it needs to be delayed at least once more.
      sample_sdi_sreg <= sample_sdi_sreg(sample_sdi_sreg'left - 1 downto 0) & sclk_internal_leading_edge_reg;
      sample_sdi      <= sample_sdi_sreg(n_clks_rx_sample_strobes_delay);

      sample_sdi_reg <= sample_sdi;
    end if;

  end process p_delay_sample_sdi;

  ---------------------------------------------------------------------------
  -- Sample the data coming from the peripheral.
  ---------------------------------------------------------------------------

  p_receive_from_peripheral : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (sample_sdi) then
        d_from_peripheral <= d_from_peripheral(d_from_peripheral'left - 1 downto 0) & sd_from_peripheral_reg;
      end if;
    end if;

  end process p_receive_from_peripheral;

end architecture arch;

