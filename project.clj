(defproject hamlet "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.3"]
		 [ring/ring-core "1.2.0"]
                 [ring/ring-jetty-adapter "1.2.0"]]
  :plugins [[lein-ring "0.8.7"]]
  :ring {:handler hamlet.core/handler
         :auto-reload? true})
