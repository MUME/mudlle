  cvt_directory = fn (dir)
    [
      | files |

      files = directory_files(dir);
      dir = dir + "/";
      lforeach
	(fn (dfile)
	 [
	   | path |

	   if (!temp_name(dfile) && file_isfile?(path = dir + dfile))
	     save_data("new/" + path, load_data_debug(path))
	 ], files);
      lforeach
	(fn (dfile)
	 [
	   | path |

	   if (!temp_name(dfile) && file_isdirectory?(path = dir + dfile))
	     [
	       mkdir("new/" + path, 7 * 64 + 7 * 8 + 7);
	       cvt_directory(path);
	     ]
	 ], files);
   ];


  // File functions

  file_isfile? = fn (name)
    [
      | sb |

      (sb = file_stat(name)) && stat_isfile?(sb)
    ];

  stat_isfile? = fn (sb) (sb[2] & 61440) == 32768;

  file_isdirectory? = fn (name)
    [
      | sb |

      (sb = file_stat(name)) && stat_isdirectory?(sb)
    ];

  stat_isdirectory? = fn (sb) (sb[2] & 61440) == 16384;

  temp_name = fn (name)
    [
      | l, c |

      c = name[0];
      if (c == ?# || c == ?.) exit<function> true;

      l = string_length(name);
      c = name[l - 1];
      if (c == ?~ || c == ?%) exit<function> true;
      if (string_cmp(name, "core") == 0) exit<function> true;

      false
    ];
