package Tests;

public class A {
    int m;
    public A (int m) {
    }
    A(){}
    
    int a() {
		return 3;
	}
	

}

class B {
    B(){
        int m = 123;
        A x = new A(m);
        x.a();
        int y = true;
    }

    public static void main(String[] args) {

    }
}