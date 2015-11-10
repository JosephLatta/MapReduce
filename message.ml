open MapReduce

type message = (app * app.input *
   (app.k *app.map_out list) * ControllerMain.job_type)
include Protocol.Make(struct type t = message end)
