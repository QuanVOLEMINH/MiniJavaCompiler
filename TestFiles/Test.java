/************************
 *******Author***********
 * - Quan
 * - Selina
 * - Thomas
 * - Weixin
 */

public class SimpleClass{
    public int int_num1=1, int_num2;
    int int_num3, int_num4;
    boolean boolnum = false;
    float float_num;

    private class InnerClass{
        int inner_int = (1+2+3)|9;
    }

    void myMethod(int a){
        int test;
        test = 1*1+1-1;
        if(test--){
            test++;
        }else{
            test--;
        }
    }

    void anotherMethod(double d, float f){
        d >>= 3;
    };
    
}


