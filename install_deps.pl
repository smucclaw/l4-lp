:- Opts = [interactive(false), test(false)],
  pack_install(date_time, [version('0.1.4') | Opts]),
  pack_install(clpBNR, [version('0.11.8') | Opts]),
  halt.