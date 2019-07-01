package Tests;

public class A {
    int m;
    public A (int m) {
        this.m = m;
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
        int[] data = new int[] {10,20,30,40,50,60,71,80,90,91};
        int[][][] x = new int[1][][]{{1,2},{1,2}};
    }

    public static void main(String[] args) {

    }
}