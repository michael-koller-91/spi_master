library ieee;
  use ieee.math_real.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_1164.all;

library spi_lib;
  use spi_lib.spi_package.all;

entity spi_master is
  generic (
    g_config : t_config := C_DEFAULT_CONFIG
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
    i_d_to_peripheral               : in    std_ulogic_vector(G_CONFIG.max_n_bits - 1 downto 0) := (others => '0');
    o_d_from_peripheral             : out   std_ulogic_vector(G_CONFIG.max_n_bits - 1 downto 0) := (others => '0');
    o_d_from_peripheral_read_strobe : out   std_ulogic                                          := '0'; -- use this to sample o_d_from_peripheral
    -- settings
    i_settings : in    t_settings(
      sclk_divide_half_minus_1 (ceil_log2(G_CONFIG.max_sclk_divide_half) - 1 downto 0),
      n_bits_minus_1 (ceil_log2(G_CONFIG.max_n_bits) - 1 downto 0),
      n_clks_scs_to_sclk_minus_1 (ceil_log2(G_CONFIG.max_n_clks_scs_to_sclk) - 1 downto 0),
      n_clks_sclk_to_scs_minus_1 (ceil_log2(G_CONFIG.max_n_clks_sclk_to_scs) - 1 downto 0),
      n_clks_sclk_to_le_minus_1 (ceil_log2(G_CONFIG.max_n_clks_sclk_to_le) - 1 downto 0),
      n_clks_le_width_minus_1 (ceil_log2(G_CONFIG.max_n_clks_le_width) - 1 downto 0),
      n_clks_rx_sample_strobes_delay (ceil_log2(G_CONFIG.max_n_clks_rx_sample_strobes_delay + 1) - 1 downto 0)
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

  signal counter_clk_divide         : natural range 0 to G_CONFIG.max_sclk_divide_half - 1   := 0;
  signal counter_n_sclk_edges       : natural range 0 to 2 * G_CONFIG.max_n_bits - 1         := 2 * G_CONFIG.max_n_bits - 1;
  signal counter_n_clks_sclk_to_scs : natural range 0 to G_CONFIG.max_n_clks_sclk_to_scs - 1 := G_CONFIG.max_n_clks_sclk_to_scs - 1;
  signal counter_n_clks_scs_to_sclk : natural range 0 to G_CONFIG.max_n_clks_scs_to_sclk - 1 := G_CONFIG.max_n_clks_scs_to_sclk - 1;
  signal counter_n_clks_sclk_to_le  : natural range 0 to G_CONFIG.max_n_clks_sclk_to_le - 1  := G_CONFIG.max_n_clks_sclk_to_le - 1;
  signal counter_n_clks_le_width    : natural range 0 to G_CONFIG.max_n_clks_le_width - 1    := G_CONFIG.max_n_clks_le_width - 1;
  signal counter_n_sample_sdi       : natural range 0 to G_CONFIG.max_n_bits                 := G_CONFIG.max_n_bits;

  signal sample_strobes_delay_reg : std_ulogic_vector(G_CONFIG.max_n_clks_rx_sample_strobes_delay downto 1) := (others => '0');

  signal sclk : std_ulogic := '1';
  signal scs  : std_ulogic := '1';

  signal sclk_edge           : std_ulogic := '0';
  signal sample_sdi          : std_ulogic := '0';
  signal sample_sdi_no_delay : std_ulogic := '0';
  signal sample_sdi_d        : std_ulogic := '0';
  signal sample_sdo          : std_ulogic := '0';

  type t_state is (idle, wait_sclk, trx, wait_scs_and_le_and_sample_sdi);

  signal state : t_state := idle;

  signal le        : std_ulogic := '0';
  signal enable_le : std_ulogic := '0';

  signal busy       : std_ulogic := '0';
  signal ready      : std_ulogic := '0';
  signal reset_sclk : std_ulogic := '1';

  signal streaming_start : std_ulogic := '0';

  -- sampled control
  signal start          : std_ulogic := '0';
  signal keep_streaming : std_ulogic := '0';

  -- sampled settings
  signal streaming_mode                          : std_ulogic                                                     := '0';
  signal sclk_divide_half_minus_1                : natural range 0 to G_CONFIG.max_sclk_divide_half - 1           := 0;
  signal transmit_on_sclk_edge_toward_idle_state : std_ulogic                                                     := '1';
  signal n_clks_sclk_to_scs_minus_1              : natural range 0 to G_CONFIG.max_n_clks_sclk_to_scs - 1         := 0;
  signal n_clks_sclk_to_le_minus_1               : natural range 0 to G_CONFIG.max_n_clks_sclk_to_le - 1          := 0;
  signal n_clks_le_width_minus_1                 : natural range 0 to G_CONFIG.max_n_clks_le_width - 1            := 0;
  signal n_clks_rx_sample_strobes_delay          : natural range 0 to G_CONFIG.max_n_clks_rx_sample_strobes_delay := 0;

  -- data
  signal d_to_peripheral   : std_ulogic_vector(G_CONFIG.max_n_bits - 1 downto 0) := (others => '0');
  signal d_from_peripheral : std_ulogic_vector(G_CONFIG.max_n_bits - 1 downto 0) := (others => '0');

begin

  -- block start if busy
  start <= i_start and not busy;

  p_sample_settings : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (start = '1') then
        streaming_mode                          <= i_settings.streaming_mode;
        transmit_on_sclk_edge_toward_idle_state <= i_settings.transmit_on_sclk_edge_toward_idle_state;
        sclk_divide_half_minus_1                <= to_integer(i_settings.sclk_divide_half_minus_1);
        n_clks_sclk_to_scs_minus_1              <= to_integer(i_settings.n_clks_sclk_to_scs_minus_1);
        n_clks_sclk_to_le_minus_1               <= to_integer(i_settings.n_clks_sclk_to_le_minus_1);
        n_clks_le_width_minus_1                 <= to_integer(i_settings.n_clks_le_width_minus_1);
        n_clks_rx_sample_strobes_delay          <= to_integer(i_settings.n_clks_rx_sample_strobes_delay);
      end if;
    end if;

  end process p_sample_settings;

  keep_streaming    <= i_keep_streaming;
  o_streaming_start <= streaming_start;

  p_delay_sample_sdi : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      sample_sdi_d <= sample_sdi;
    end if;

  end process p_delay_sample_sdi;

  o_d_from_peripheral <= d_from_peripheral;

  o_d_from_peripheral_read_strobe <= sample_sdi_d when counter_n_sample_sdi = 0 else
                                     '0';

  o_busy  <= busy;
  o_ready <= ready;

  o_sclk <= sclk;
  o_scs  <= scs;
  o_le   <= le;

  p_fsm : process (i_clk) is
  begin

    if rising_edge(i_clk) then

      case state is

        when wait_sclk =>

          if (counter_n_clks_scs_to_sclk = 1) then
            reset_sclk <= '0';
            state      <= trx;
          else
            counter_n_clks_scs_to_sclk <= counter_n_clks_scs_to_sclk - 1;
          end if;

        when trx =>

          streaming_start <= '0';
          if (counter_n_sclk_edges = 0) then
            if (counter_clk_divide = 0) then
              streaming_start      <= '1';
              counter_n_sclk_edges <= to_integer(i_settings.n_bits_minus_1 & '1');
              if not(streaming_mode = '1' and keep_streaming = '1') then
                reset_sclk <= '1';
                state      <= wait_scs_and_le_and_sample_sdi;
              end if;
              counter_n_clks_sclk_to_scs <= n_clks_sclk_to_scs_minus_1;
              counter_n_clks_sclk_to_le  <= n_clks_sclk_to_le_minus_1;
              counter_n_clks_le_width    <= n_clks_le_width_minus_1;
            end if;
          else
            if (counter_clk_divide = 0) then
              counter_n_sclk_edges <= counter_n_sclk_edges - 1;
            end if;
          end if;

        when wait_scs_and_le_and_sample_sdi =>

          if (counter_n_clks_sclk_to_scs = 0) then
            scs <= i_settings.scs_idle_state;
          else
            counter_n_clks_sclk_to_scs <= counter_n_clks_sclk_to_scs - 1;
          end if;

          if (enable_le = '1') then
            if (counter_n_clks_sclk_to_le = 0) then
              le        <= '1';
              enable_le <= '0';
            else
              counter_n_clks_sclk_to_le <= counter_n_clks_sclk_to_le - 1;
            end if;
          end if;

          if (le = '1') then
            if (counter_n_clks_le_width = 0) then
              le <= '0';
            else
              counter_n_clks_le_width <= counter_n_clks_le_width - 1;
            end if;
          end if;

          if (counter_n_clks_sclk_to_scs = 0 and counter_n_clks_le_width = 0 and counter_n_sample_sdi = 0) then
            ready <= '1';
            state <= idle;
          end if;

        when others =>                                                                                          -- idle

          busy  <= '0';
          ready <= '0';
          scs   <= i_settings.scs_idle_state;                                                                   -- make in-port change visible at out-port without the need of a start strobe
          if (start = '1') then
            -- `counter_n_sclk_edges` counts 2 * n_bits sclk edges:
            --    from 2 * n_bits - 1 downto 0
            --    <=> from 2 * (n_bits - 1) + 1 downto 0
            counter_n_sclk_edges <= to_integer(i_settings.n_bits_minus_1 & '1');
            --
            counter_n_clks_scs_to_sclk <= to_integer(i_settings.n_clks_scs_to_sclk_minus_1);
            scs                        <= not i_settings.scs_idle_state;
            --
            enable_le <= '1';
            --
            busy <= '1';

            if (i_settings.n_clks_scs_to_sclk_minus_1 = 0) then
              reset_sclk <= '0';
              state      <= trx;
            else
              state <= wait_sclk;
            end if;
          end if;

      end case;

    end if;

  end process p_fsm;

  p_generate_sample_strobes : process (all) is
  begin

    if (transmit_on_sclk_edge_toward_idle_state = '1') then
      if (i_settings.sclk_idle_state = '1') then
        sample_sdo          <= sclk_edge and sclk;
        sample_sdi_no_delay <= sclk_edge and not sclk;
      else
        sample_sdo          <= sclk_edge and not sclk;
        sample_sdi_no_delay <= sclk_edge and sclk;
      end if;
    else
      if (i_settings.sclk_idle_state = '1') then
        sample_sdo          <= sclk_edge and not sclk;
        sample_sdi_no_delay <= sclk_edge and sclk;
      else
        sample_sdo          <= sclk_edge and sclk;
        sample_sdi_no_delay <= sclk_edge and not sclk;
      end if;
    end if;

  end process p_generate_sample_strobes;

  generate_sample_sdi_strobes : if G_CONFIG.max_n_clks_rx_sample_strobes_delay = 0 generate
    sample_sdi <= sample_sdi_no_delay;
  else generate

    p_delay_sample_sdi_strobes : process (i_clk) is
    begin

      if rising_edge(i_clk) then
        sample_strobes_delay_reg <= sample_strobes_delay_reg(sample_strobes_delay_reg'left - 1 downto sample_strobes_delay_reg'right) & sample_sdi_no_delay;
      end if;

    end process p_delay_sample_sdi_strobes;

    p_multiplex_sample_sdi_strobes : process (all) is
    begin

      if (n_clks_rx_sample_strobes_delay = 0) then
        sample_sdi <= sample_sdi_no_delay;
      else
        sample_sdi <= sample_strobes_delay_reg(n_clks_rx_sample_strobes_delay);
      end if;

    end process p_multiplex_sample_sdi_strobes;

  end generate generate_sample_sdi_strobes;

  p_count_sample_sdi_strobes : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (start = '1' or streaming_start = '1') then
        counter_n_sample_sdi <= to_integer(i_settings.n_bits_minus_1) + 1;
      else
        if (sample_sdi = '1') then
          counter_n_sample_sdi <= counter_n_sample_sdi - 1;
        end if;
      end if;
    end if;

  end process p_count_sample_sdi_strobes;

  p_generate_sclk : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (reset_sclk = '1' or (streaming_start = '1' and streaming_mode = '1')) then
        counter_clk_divide <= 0;
        sclk               <= i_settings.sclk_idle_state;
        sclk_edge          <= '0';
      else
        sclk_edge <= '0';
        if (counter_clk_divide = 0) then
          counter_clk_divide <= sclk_divide_half_minus_1;
          sclk               <= not sclk;
          sclk_edge          <= '1';
        else
          counter_clk_divide <= counter_clk_divide - 1;
        end if;
      end if;
    end if;

  end process p_generate_sclk;

  p_receive_from_peripheral : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (sample_sdi = '1') then
        d_from_peripheral(0)                               <= i_sd_from_peripheral;
        d_from_peripheral(d_from_peripheral'left downto 1) <= d_from_peripheral(d_from_peripheral'left - 1 downto 0);
      end if;
    end if;

  end process p_receive_from_peripheral;

  p_transmit_to_peripheral : process (i_clk) is
  begin

    if rising_edge(i_clk) then
      if (start = '1') then
        d_to_peripheral <= i_d_to_peripheral;
      else
        o_sd_to_peripheral <= d_to_peripheral(0);
        if (sample_sdo = '1') then
          d_to_peripheral(d_to_peripheral'left - 1 downto 0) <= d_to_peripheral(d_to_peripheral'left downto 1);
        end if;
      end if;
    end if;

  end process p_transmit_to_peripheral;

end architecture arch;

