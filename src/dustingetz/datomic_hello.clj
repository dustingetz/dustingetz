(ns dustingetz.datomic-hello
  (:require
    [datomic.client.api :as d]
    ))

(def cfg
  {:server-type :cloud
   :region      "us-east-2"
   :system      "dustingetz-hfdemo-20191119"
   :endpoint    "http://entry.dustingetz-hfdemo-20191119.us-east-2.datomic.net:8182"
   :proxy-port  8182
   })

(comment
  (def client (d/client cfg))
  (def conn (d/connect client {:db-name "movies"})))

