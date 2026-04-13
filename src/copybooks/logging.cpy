      * Copybook: Logging Control (shared)
      * Purpose: Centralized logging state and buffers
       01 log-control.
           05 log-init-done     PIC 9 VALUE 0.
           05 log-level         PIC 9 VALUE 0.
           05 log-env-val       PIC X(32).
           05 log-component     PIC X(32).
           05 log-sink          PIC X(8).
           05 log-file-path     PIC X(256).
           05 log-level-name    PIC X(5).
           05 log-min-level     PIC 9 VALUE 0.
           05 log-message       PIC X(512).
           05 log-line          PIC X(600).
           05 log-cmd           PIC X(900).
           05 log-pos           PIC 9(4) COMP-5.
           05 log-num-text      PIC X(32).
