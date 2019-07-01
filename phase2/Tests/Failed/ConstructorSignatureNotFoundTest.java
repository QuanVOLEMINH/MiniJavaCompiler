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
        A y = new A(false);
    }

    public static void main(String[] args) {

    }
}