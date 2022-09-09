{:user {:signing {:gpg-key "92BE8E65AB9248EFF68B75792EDA1D9FDE98B3BF"}

        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"]

        :dependencies [[criterium "0.4.6"]
                       [nrepl "0.6.0"]
                       [org.clojure/tools.logging "0.4.1"]
                       [pjstadig/humane-test-output "0.10.0"]]

        :plugins [[cider/cider-nrepl "0.23.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.24.1"]
                  [jonase/eastwood "0.3.6"]
                  [lein-ancient "0.6.15"]
                  [lein-bikeshed "0.5.2"]
                  [lein-cljfmt "0.6.6"]
                  [lein-kibit "0.1.8"]
                  [lein-nvd "1.3.1"]
                  [lein-pprint "1.2.0"]]

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}

 :frs [:flight-recorder :flight-recorder/sampling]
 :frt [:flight-recorder :flight-recorder/tracing]

 :flight-recorder {:jvm-opts ["-XX:+UnlockDiagnosticVMOptions"
                              "-XX:+DebugNonSafepoints"
                              "-XX:FlightRecorderOptions=stackdepth=1024"]}

 :flight-recorder/sampling {:jvm-opts ["-XX:StartFlightRecording=dumponexit=true,filename=.flight-recording.jfr,maxage=2h,settings=default"]}
 :flight-recorder/tracing  {:jvm-opts ["-XX:StartFlightRecording=dumponexit=true,filename=.flight-recording.jfr,maxage=2h,settings=profile"]}}
