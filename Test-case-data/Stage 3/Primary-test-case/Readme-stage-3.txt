incorrect-test-case-1:
======================

int [10] a;
int [5][2] b;
int [5][2] c;

b = c;
a = b		// type mismatch here ...

Sample output: Type mismatch a = b


incorrect-test-case-2:
======================

int [10] a;
int [5][2] b;
int [5][2] c;
float [10] d;

b = c;
a = d	// type mismatch here ...

Sample output: Type mismatch a = d


incorrect-test-case-3:
======================

int [10] a;
int [5][2] b;
int [5][2] c;
float [10] d;

b = c;

a[2] = d[9]	// type mismatch here ...


Sample output: Type mismatch a[2] = d[9]


incorrect-test-case-4:
======================

int [10] a;
int [5][2] b;
int [5][2] c;
float [10] d;

b = c;

a[2] = b[1][0] - 3.45	// type mismatch here ...


Sample output: Type mismatch a[2] = b[1][0] - 3.45
