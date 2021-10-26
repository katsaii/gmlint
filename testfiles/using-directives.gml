show_message

//# ALLOW banned-functions
show_message();

//# WARN banned-functions
var a = show_message;
a();

show_message2("show_message"); // show_message
@'
  show_message'
