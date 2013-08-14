struct gnttab_setup_table {
  uint16_t dom;
  uint32_t nr_frames;
  int16_t status;
  __guest_handle_ulong frame_list;
};

