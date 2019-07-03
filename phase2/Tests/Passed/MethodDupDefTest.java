package Tests.Passed;


class A extends B {

	A () {

	}

    // method overloading
	void a(String b) {}

	void a(int c){}

	void a(int c, int d){}
	
    double a(double x){
        return x;
    }

    int test(){
        return 0;
    }
}

abstract class B {
    abstract int test();
}