#!/usr/bin/zsh
  # accepts:
  #   ANSI_COLOR_ID (defaults to 1=red)
  #   HIST_BLOCK (defaults to "x")
  #   COL (-umn width, defaults to 40), and
  #   KB (SI Kilo multiplier, defaults to 1024) env vars;
  # can process its own output (verbatim or otherwise)

# top_10() customizations
: "${ANSI_COLOR_ID:=2}"
: "${HIST_BLOCK:=▬}"
: "${HIST_ANCESTRY:=🐰🍀🌷x✡♱☠❤❄◆▬■●▶}"
: "${COL:=70}"

: "${TPUT:=tput}"

perl -nale "BEGIN { \$KB=${KB-1024}; \$UNIT=-4; sub log_2 (\$) {log(shift)/log(2)} }
              use POSIX qw(strftime);
              END {
                \$DIV = \$KB**(\$UNIT);
                for (sort {\$h{\$b} <=> \$h{\$a}} keys %h) {
                  \$SCALE //= do {
                       if (\$h{\$_}/\$DIV > ${COL-40}**2) {\"log_2\"}
                    elsif (\$h{\$_}/\$DIV > ${COL-40})    {\"sqrt\"}
                    else                                  {\"\"}
                  };
                  printf \"%${COL-40}s %s %s%s\\n\",
                    \$_,
                    \"$($TPUT bold)$($TPUT setaf ${ANSI_COLOR_ID-1})${HIST_BLOCK-x}$($TPUT sgr0)\" x
                      (eval \"\$SCALE(\$h{\$_}/\$DIV)\"),
                    ((\$UNIT==0) ? do {\$a= int (\$h{\$_} / 3600) . \"h\" . (int (\$h{\$_}/60) % 60).\"m\" .
                     (int (\$h{\$_} % 60).\"s\"); s/^0h//, s/^0m// for \$a; \$a} :
                      eval          \"\$h{\$_}/\$DIV\"),
                    (\"\", map \" \$_\".(\$KB==1024 && 'i').(\"\", \"B\", \"s\")[\$UNIT<=>0],
                             qw/K M G T P E p n μ m/)[\$UNIT]
                }
              }
              next unless /\\S\\s+[+-]?[\\d.]+\\w*\\b/;
              \$F[-1] =~ /^[KMGTPEpnμm]i?[Bs]\$/ and \$F[-2] .= \$F[-1] and pop @F;
              s/[#:]\$// for @F;
              @F = grep length && !/top_10/, @F;
              (\$F[-1] =~ s/^(?:(\\d+)h)?(?:(\\d{1,2})m)?(\\d{1,2})s.*\$/(\$1||0).\"*3600+\".(\$2||0).\"*60+\$3\"/e or
                 (\$F[-2] =~ s/^(?:(\\d+)h)?(?:(\\d{1,2})m)?(\\d{1,2})s.*\$/(\$1||0).\"*3600+\".(\$2||0).\"*60+\$3\"/e and pop @F)) and

                 @F > 6 and splice @F, 4, @F-4, '...', '#', \$F[-1];
              my \$unit = 0;
              for (\$F[-1]) {
                s/E/*(\$KB**6)/    and \$unit = 6;
                s/P/*(\$KB**5)/    and \$unit = 5;
                s/T/*(\$KB**4)/    and \$unit = 4;
                s/G/*(\$KB**3)/i   and \$unit = 3;
                s/M/*(\$KB**2)/    and \$unit = 2;
                s/K/*\$KB/i        and \$unit = 1;
                s!m!/1000!         and \$unit =-1 and \$KB=1000;
                s![μu]!/(1000**2)! and \$unit =-2 and \$KB=1000;
                s!n!/(1000**3)!    and \$unit =-3 and \$KB=1000;
                s!p!/(1000**4)!    and \$unit =-4 and \$KB=1000;
                tr!0-9*/().+-!!dc;
                \$_ = eval
              }
              \$F[0] =~ s/^(\\d{8,10}):0;/(strftime '%a %b %e %H:%M:%S', localtime \$1) . \" % \"/e;
              \$UNIT = \$unit if \$unit > \$UNIT;
              \$h{+join \" \", grep !/^(?:\\Q$($TPUT bold)\\E[^$HIST_ANCESTRY]*?[$HIST_ANCESTRY]+\\Q$($TPUT sgr0)\\E)+$/, @F[0..(\$#F-1)]} += \$F[-1]" | head "${@:-${TOP_10_ARGS:--10}}"
