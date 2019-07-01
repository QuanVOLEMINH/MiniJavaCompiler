package Tests;

public class A {
    int m;
    public A (int m) {
        this.m = m;
    }
    A(){}
    
    int a(String l[]) {
		return 3;
	}
	

}

class B {
    B(){
        int m = 123;
        A x = new A(m);
        x.a();
        int x = true;
    }

    public static void main(String[] args) {

    }
}