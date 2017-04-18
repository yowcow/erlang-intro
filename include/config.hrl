-record(person, {myid, gender, age, zip = 0}).

-define(MYID, 12345).
-define(GENDER, f).
-define(AGE, 99).
-define(PERSON, #person{age = ?AGE, gender = ?GENDER, myid = ?MYID}).
