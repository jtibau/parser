
incorrect-test-case-1:
======================


struct {
	int a;
	float b;
} X;

struct {
	int a;
	float b;	
	bool b1;
} Y;

Y = X;		// type error because the structure fields do not match (<bool b1> is missing in <X>)
X = Y

Sample output: Type mismatch Y = X



incorrect-test-case-3:
======================

struct {
	int a;
	int b;
	int c;
	bool d;
	string s1;
	float f;
} X;

struct {
	int a;
	int b;
	int c;
	bool d;
	string s2;
	float f;
} Y;

X = Y 	// type error because of mismatch in the string fleild's name <s1> and <s2>

Sample output: Type mismatch X = Y


incorrect-test-case-5:
======================

struct {
	string s1;
	float y;
	int b1;
} A, A1, a123;

struct {
	string s1;
	int y;
	int b1;
} B, B2, B234;

A.y = (23.821-60+3.56);
B2.s1 = "Hello";
B234.y = 49 * B.y * 3 / (B.y-(B2.y+123));
A = A1 + B2 - a123		// Type mismatch here ... +/- operator not allowed on struct

Sample output: Type mismatch A = A1 + B2 - a123 =OR= Type mismatch A1 + B2 - a123 =OR= Type mismatch A1 + B2


incorrect-test-case-6:
======================

int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;

struct {
	string s1;
	float y;
	int b1;
} A, A1, a123;

struct {
	string s1;
	int y;
	int b1;
} B, B2, B234;

A.y = (23.821-60+3.56);
B2.s1 = "Hello";
B234.y = 49 * B.y * 3 / (B.y-(B2.y+123));

if B <> A then	// type mismatch here ...
	A.s1 = x
else
	bool1 = true;

z = e

Sample output: Type mismatch B <> A



incorrect-test-case-8:
======================


int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;


struct {
	string s1;
	float y;
	int b1;
} A, A1, a123;

struct {
	string s1;
	int y;
	int b1;
} B, B2, B234;

A.y = (23.821-60+3.56);
B2.s1 = "Hello";
B234.y = 49 * B.y * 3 / (B.y-(B2.y+123));

if (A) >= A1 then		// struct can not be compared using >= operator
	A.s1 = B.s1 + s2 + s3
else
	bool1 = bool2;

z = e

Sample output: Type mismatch (A) >= A1 =OR= Type mismatch if (A) >= A1 then


incorrect-test-case-10:
=======================

struct {
	int a;
	int b;
} st1;

struct {
	int a;
	int b;
	int c;
} st2;

st1.a = st2.a;
st2.c = st2.c - st1.b;
st1.c = st2.c;		// type mismatch here ... st1.c does not exist!


Sample output: Type mismatch st1.c = st2.c

