{application,blockparser,
             [{description,"Bitcoin disk block parser"},
              {vsn,"1"},
              {registered,[blockhandler]},
              {applications,[kernel,stdlib,poolboy]},
              {mod,{blockparser,[]}},
              {env,[]},
              {modules,[blockparser,blockparser_sup,blockparser_worker]}]}.
