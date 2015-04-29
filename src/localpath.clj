(ns localpath)
;machine specific path for configuration and data files.
(def ldir
  (if (.equalsIgnoreCase (java.lang.System/getProperty "os.name") "Windows 7")
    "C:\\Users\\username\\data\\"
    "data/"
    )
  )


