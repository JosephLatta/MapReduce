# MapReduce
Distributed, asynchronous MapReduce Framework using OCaml

USERS: There are a couple notes things to know when running this software.
You must have all the apps you would like to run in the same folder as 
this Documentation in order for it to work properly. You must remember to add
your servers to Addresses.txt before running the program. 
USAGE: [run] controllerMain.ml <application name> <filename>" in your 
terminal to run the software. This implementation only supports the passing
of a single file.

APP MAKERS: There are several important things to note when implementing new
apps for this framework. First, every app must implement the App signature
found in MapReduce.ml. Note that your read function must return a list of 
values that will each be mapped using your map function. Also, make sure to
call "let () = MapReduce.register_app (module App)" at the end of your
module.

DEVELOPERS: Most of the heart of the MapReduce functionality is unsurprisingly
found in mapReduce.ml. You can see here the necessary functions and types that 
every app must implement. The controllerMain.ml file is the entry point and
delegates tasks to "workers" which are identified in the adressess.txt file. 
