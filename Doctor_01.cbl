IDENTIFICATION DIVISION.
            PROGRAM-ID. Doctor_01.
            AUTHOR. Doctor, Amherstia S.
            
        DATA DIVISION.
            WORKING-STORAGE SECTION.
            77 CHOICE PIC 9.
            77 EXITED PIC 9 VALUE 6.
            77 COUNTER PIC 9 VALUE 0.
        *temporary variables to store input in
            77 studName PIC X(50).
            77 studNo PIC X(10).
            77 studCourse PIC X(50).
            77 studAge PIC 99.
            77 mobNo PIC X(11).
            77 landNo PIC X(8).
        *array of students            
            01 students.
              02 student-info OCCURS 5 TIMES INDEXED BY i.
                03 fullname PIC X(50).
                03 SNo PIC X(10).
                03 course PIC X(50).
                03 contact-no.
                  04 mobile PIC X(11).
                  04 landline PIC X(8).
                03 age PIC 99.
                
        PROCEDURE DIVISION.
            PERFORM MENU UNTIL EXITED = 6.
            STOP RUN.
            
            SEARCH-ARRAY.
               IF studNo = SNo(i)
                DISPLAY "STUDENT FOUND"
                
            ADD-STUDENT.
               DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING
               ACCEPT studNo
               MOVE studNo TO SNo(COUNTER)
               
               DISPLAY "ENTER STUDENT'S FULL NAME: " WITH NO ADVANCING
               ACCEPT studName
               MOVE studName TO fullname(COUNTER)
               
               DISPLAY "ENTER STUDENT COURSE: " WITH NO ADVANCING
               ACCEPT studCourse
               MOVE studCourse to course(COUNTER)
               
               DISPLAY "ENTER STUDENT AGE: " WITH NO ADVANCING
               ACCEPT studAge
               MOVE studAge to age(COUNTER)
               
               DISPLAY "ENTER CONTACT NUMBER DETAILS:"
               DISPLAY "ENTER MOBILE NO.: " WITH NO ADVANCING
               ACCEPT mobNo
               MOVE mobNo to contact-no(COUNTER, 1)
               DISPLAY "ENTER LANDLINE NO.: " WITH NO ADVANCING
               ACCEPT landNo
               MOVE landNo to contact-no(COUNTER, 2)
               
               COMPUTE i = i + 1               
        *incrementing number of students added                
               COMPUTE COUNTER = COUNTER + 1
               
            EDIT-STUDENT.
              IF COUNTER != 0
                DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING
                ACCEPT studNo
               
                PERFORM SEARCH-ARRAY COUNTER TIMES
                
                DISPLAY "ENTER COURSE: " WITH NO ADVANCING
                ACCEPT studCourse
                MOVE studCourse TO course(i)
                
                DISPLAY "ENTER AGE: " WITH NO ADVANCING
                ACCEPT studAge
                MOVE studAge TO age(i)
                
                DISPLAY "ENTER MOBILE NO.: " WITH NO ADVANCING
                ACCEPT mobNo
                MOVE mobNo TO mobile(i)
                DISPLAY "ENTER LANDLINE NO.: " WITH NO ADVANCING
                ACCEPT landNo
                MOVE landNo TO landline(i)
                
              ELSE
                DISPLAY "THE ARRAY IS EMPTY"
              END-IF.
              
            DELETE-STUDENT.
              IF COUNTER = 0
               DISPLAY "THE ARRAY IS EMPTY"
              ELSE
               PERFORM SEARCH-ARRAY COUNTER TIMES
               
               MOVE fullname(i + 1) TO fullname(i)
               MOVE SNo(i + 1) TO SNo(i)
               MOVE course(i + 1) TO course(i)
               MOVE mobile(i + 1) TO mobile(i)
               MOVE landline(i + 1) TO landline(i)
               
               COMPUTE i = i - 1
               COMPUTE COUNTER = COUNTER - 1
               
               END-IF.
               
            VIEW-STUDENT.
              IF COUNTER = 0
               DISPLAY "THE ARRAY IS EMPTY"
              ELSE
                PERFORM SEARCH-ARRAY COUNTER TIMES
                
                DISPLAY "NAME: " WITH NO ADVANCING
                DISPLAY fullname(i)
                DISPLAY "COURSE: " WITH NO ADVANCING
                DISPLAY course(i)
                DISPLAY "AGE: " WITH NO ADVANCING
                DISPLAY age(i)
                DISPLAY "CONTACT DETAILS:"
                DISPLAY "MOBILE NO.: " WITH NO ADVANCING
                DISPLAY mobile(i)
                DISPLAY "LANDLINE NO.: " landline(i)
                
            VIEW-ALL.
                DISPLAY "NAME: " fullname(i)
                DISPLAY "COURSE: " course(i)
                DISPLAY "AGE " age(i)
                DISPLAY "CONTACT DETAULS: "
                DISPLAY "MOBILE NO.: " mobile(i)
                DISPLAY "LANDLINE NO.: " landline(i)
            
            MENU.
              DISPLAY "MENU".
              DISPLAY "[1] ADD STUDENT".
              DISPLAY "[2] EDIT STUDENT".
              DISPLAY "[3] DELETE STUDENT".
              DISPLAY "[4] VIEW STUDENT".
              DISPLAY "[5] VIEW ALL STUDENTS".
              DISPLAY "CHOICE: " WITH NO ADVANCING.
              ACCEPT CHOICE.
            
            IF CHOICE = 1
               PERFORM ADD-STUDENT
            ELSE
              IF CHOICE = 2
               PERFORM EDIT-STUDENT
              ELSE
                IF CHOICE = 3
                  PERFORM DELETE-STUDENT
                ELSE
                  IF CHOICE = 4
                    PERFORM VIEW-STUDENT
                  ELSE
                    IF CHOICE = 5
                      PERFORM VIEW-ALL VARYING i FROM 1 BY 1 UNTIL i = COUNTER
                  END-IF.
                END-IF.
              END-IF.
            END-IF.