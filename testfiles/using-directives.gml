show_message

//# ALLOW illegal-functions
show_message();

//# WARN illegal-functions
var a = show_message;
a();
