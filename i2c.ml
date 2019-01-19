let char_of_hex s = Scanf.sscanf s "0x%x" (fun n -> Char.chr n)

type t = {
  time: float;
  packet_id: int;
  address: char;
  data: char;
  rw: [`Read | `Write];
  ack: bool;
}

let parse_row = function
  | [time; packet_id; address; data; rw; ack] ->
    let time = float_of_string time in
    let packet_id = int_of_string packet_id in
    let address = char_of_hex address in
    let data = char_of_hex data in
    let rw =
      match rw with
      | "Read" -> `Read
      | "Write" -> `Write
      | s -> failwith @@ Printf.sprintf "expected Read/Write, got %s" s
    in
    let ack =
      match ack with
      | "ACK" -> true
      | "NAK" -> false
      | s -> failwith @@ Printf.sprintf "expected ACK/NAK, got %s" s
    in
    {time; packet_id; address; data; rw; ack}
  | l ->
    failwith @@ Printf.sprintf "expected 6 fields, got %d" (List.length l)


(* FIXME move this out to allow using different back-ends *)
let c_of_bool = function
  | true -> "1"
  | false -> "0"
let start addr rw =
  Printf.printf "cmd = i2c_cmd_link_create();\n";
  Printf.printf "i2c_master_start(cmd);\n";
  Printf.printf "i2c_master_write_byte(cmd, (%d << 1) | %s, 1);\n"
    (Char.code addr)
    (match rw with
      | `Read -> "I2C_MASTER_READ"
      | `Write -> "I2C_MASTER_WRITE")
let stop () =
  Printf.printf "i2c_master_stop(cmd);\n";
  Printf.printf "r = i2c_master_cmd_begin(I2C_NUM_0, cmd, DELAY);\n";
  Printf.printf "i2c_cmd_link_delete(cmd);\n";
  Printf.printf "cmd = NULL; //avoid double free\n";
  Printf.printf "if (r == ESP_OK) printf(\"!\"); else printf(\".\");\n"
let write b ack =
  Printf.printf "i2c_master_write_byte(cmd, %d, %s);\n"
    (Char.code b)
    (c_of_bool ack)
let delay t =
  Printf.printf "vTaskDelay(%d / portTICK_PERIOD_MS);\n"
    (int_of_float (t *. 1000.))
let read ack =
  Printf.printf "i2c_master_read(&read, 1, %s);\n"
    (if ack then "ACK" else "NAK")

(* TODO pack bytes *)
let () =
  Printf.printf "#include \"config.h\"\n";
  Printf.printf "#include <freertos/FreeRTOS.h>\n";
  Printf.printf "#include <freertos/task.h>\n";
  Printf.printf "#include <driver/i2c.h>\n";
  Printf.printf "\n";
  Printf.printf "void app_main() {\n";
  Printf.printf "int i2c_master_port = I2C_NUM_0;\n";
  Printf.printf "i2c_config_t conf;\n";
  Printf.printf "conf.mode = I2C_MODE_MASTER;\n";
  Printf.printf "conf.sda_io_num = I2C_MASTER_SDA_IO;\n";
  Printf.printf "conf.sda_pullup_en = GPIO_PULLUP_ENABLE;\n";
  Printf.printf "conf.scl_io_num = I2C_MASTER_SCL_IO;\n";
  Printf.printf "conf.scl_pullup_en = GPIO_PULLUP_ENABLE;\n";
  Printf.printf "conf.master.clk_speed = I2C_MASTER_FREQ_HZ;\n";
  Printf.printf "i2c_param_config(i2c_master_port, &conf);\n";
  Printf.printf "i2c_driver_install(i2c_master_port, conf.mode, 0, 0, 0);\n";
  Printf.printf "\n";
  Printf.printf "i2c_cmd_handle_t cmd;\n";
  Printf.printf "esp_err_t r;\n";
  Printf.printf "char read;\n";
  Printf.printf "\n";
  let _ =
    Csv.of_channel ~has_header:true stdin |>
    Csv.fold_left ~init:None ~f:(fun prev row ->
      let {time; packet_id; address; data; rw; ack} = parse_row row in
      let do_rw () =
        match rw with
        | `Read -> read ack
        | `Write -> write data ack
      in
      match prev with
      | None ->
        (* first *)
        start address rw;
        do_rw ();
        Some (time, packet_id)
      | Some (time', packet_id') when packet_id <> packet_id' ->
        (* end, start *)
        stop ();
        delay (time -. time');
        start address rw;
        do_rw ();
        Some (time, packet_id)
      | _ ->
        (* continue *)
        do_rw ();
        Some (time, packet_id)
    )
  in
  stop ();
  Printf.printf "while (1) "; delay 1.;
  Printf.printf "}\n"
