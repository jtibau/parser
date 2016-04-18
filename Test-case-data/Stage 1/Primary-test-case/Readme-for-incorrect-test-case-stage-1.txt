
incorrect-test-case-5:
======================



int a, b,c, d;
	
b = 49.54 * a * 3 / (c-(d+123)) // type mismatch here ...


incorrect-test-case-10:
=======================


int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;

if b <> c then
	z1 = (x+y-z) + (b*c+3)
else 
	if s2 then		// type mismatch here ...
		bool1 = s2
	else
		s2 = s1 - s3
	



incorrect-test-case-11:
=======================

int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;

if b <> c then
	z1 = (x+y-z) + (b*c+3)
else 
	if bool2 then
		bool1 = s2	// type mismatch here ...
	else
		s2 = s1 - s3
	


incorrect-test-case-12:
=======================

int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;

if b <> c then
	z1 = (x+y-z) + (b*c+3)
else 
	if bool2 then
		s1 = s2
	else
		s2 = s1 - s3	// type mismatch here ...
	

additional-incorrect-test-case-1:
=================================

int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;

if bool1 then
	bool1 = bool2
else
	s2 = s1 + s3;

s1 = "Hello";
x1 = 32.81;

if b <> bool1 then		// type mismatch here ...
	x1 = (x+y-z) + (b*c+3)
else 
	if bool1 == bool2 then
		if (x1-5) >= (3+y) then
			y = 3
		else 
			s3 = "Hi, how are you?"
	else
		e = b + c;

z = e;



if z1 <= 5 then
	b = 49 * a * 3 / (c-(d+123))
else
	d = c-b;


if b < a then
	y1 = (x-z) + (b/c-3)
else 
	if (x+7) > (6-y1) then
		e = 3
	else 
		s3 = "Hi, I am a string"



additional-incorrect-test-case-2:
=================================


int a, b,c, d;
float x, x1, y, y1, z, z1;
bool bool1, bool2;
string s1, s2, s3;
int e;

if bool1 then
	bool1 = bool2
else
	s2 = s1 + s3;

s1 = "Hello";
x1 = 32.81;

if b <> a then		
	x1 = (x+y-z) + (b*c+3)
else 
	if bool1 == bool2 then
		if (x1-5) >= (s1+s2) then	// type mismatch here ...
			y = 3
		else 
			s3 = "Hi, how are you?"
	else
		e = b + c;

z = e;



if z1 <= 5 then
	b = 49 * a * 3 / (c-(d+123))
else
	d = c-b;


if b < a then
	y1 = (x-z) + (b/c-3)
else 
	if (x+7) > (6-y1) then
		e = 3
	else 
		s3 = "Hi, I am a string"
