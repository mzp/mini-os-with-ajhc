struct xencons_interface {
  char[] in;
  char[] out;
  uint32_t in_cons;
  uint32_t in_prod;
  uint32_t out_cons;
  uint32_t out_prod;
};
struct xenbus_event {
};

struct consfront_dev {
  domid_t dom;
  struct xencons_interface* ring;
  grant_ref_t ring_ref;
  evtchn_port_t evtchn;
  char* nodename;
  char* backend;
  struct xenbus_event* events;
};
