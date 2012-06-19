/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 */

static int
librdf_ocaml_logger_handler(void *user_data, librdf_log_message *log_msg)
{
  raptor_locator* locator = log_msg->locator;
  int line = -1;
  int column = -1;
  int byte = -1;
  const char *uri = NULL;
  const char *file = NULL;
  
  if (locator) {
    line = raptor_locator_line(locator);
    column = raptor_locator_column(locator);
    byte = raptor_locator_byte(locator);
    file = raptor_locator_file(locator);
    uri = raptor_locator_uri(locator);
  }

  /*  
  if (librdf_ocaml_callback)
    return librdf_call_ocaml_message(log_msg->code, log_msg->level, 
				     log_msg->facility, log_msg->message,
				     line, column, byte, file, uri);
  */

  if (log_msg->level < LIBRDF_LOG_WARN)
    return 1;

  /*
  return librdf_ocaml_message_handler((log_msg->level < LIBRDF_LOG_ERROR),
                                       log_msg->message);
  */
  fprintf(stderr, "[camlrdf] %s\n", log_msg->message);
  return 1;
}

void
librdf_ocaml_world_init(librdf_world *world)
{
  librdf_world_set_logger(world, NULL, librdf_ocaml_logger_handler);
}
