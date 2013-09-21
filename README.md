# sb

A Clojure library designed to ... well, that part is up to you.

## Usage

Steps:
* Install leiningen (https://github.com/technomancy/leiningen)
* Download the zip containing the sources of this project
* Copy the data directory from Dropbox.
* change src/localpath.clj to reflect the path of the data directory
* Run "lein uberjar", which generates a jar with all dependencies in the target folder.
* Run "java -jar target/cloimpl-0.1.0-SNAPSHOT-standalone.jar < sbo_interleaved.txt" (you can find sbo_interleaved.txt in the data folder)

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
