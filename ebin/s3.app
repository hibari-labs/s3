{application,s3,
             [{description,"Gemini S3"},
              {vsn,"0.1.1"},
              {registered,[]},
              {applications,[kernel,stdlib,sasl,inets]},
              {mod,{s3_app,[]}},
              {modules,[s3_app, s3_sup, s3, s3_client,
                        s3_utils]},
              {env,[{s3_conf_path,[]}]}]}.
