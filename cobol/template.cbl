       identification division.
       program-id. template.

       environment division.
       input-output section.
       file-control.

           select readfile
               assign to readfile-name
               file status is readfile-status
               organization is line sequential.

       data division.
       file section.
       fd  readfile.
       01  readline pic x(1024).

       working-storage section.

       01  readfile-name pic x(255).
       01  readfile-status pic x(2).

       01  templine pic x(1024).

       01  the-var           pic x(100).
       01  what-we-change    pic x(100).

       01 counter    PIC 9(4).


       linkage section.

       01 the-vars.

          03  COW-vars OCCURS 99 times.
        
            05 COW-varname       pic x(99).
            05 COW-varvalue      pic x(99).

       01 template-filename     pic x(255).                 


       procedure division using the-vars template-filename.

       move 
          function concatenate("views/",function trim(template-filename))
          to readfile-name.

       start-readfile.

           open input readfile

           call 'checkfilestatus' using readfile-name readfile-status

           read readfile

           perform until readfile-status = '10'
           
           move function trim(readline) to templine
               
               PERFORM VARYING counter FROM 1 BY 1 UNTIL counter > 99

                   move 
                      function concatenate(
                        '{{' function trim(COW-varname(counter)) '}}'
                        )
                      to 
                      what-we-change

                   move
                      function SUBSTITUTE(
                        templine, 
                        function trim(what-we-change), 
                        function trim(COW-varvalue(counter)))
                       to templine 

               END-PERFORM

               display function trim(templine)


               read readfile
           end-perform

           close readfile.
           