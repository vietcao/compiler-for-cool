import java.io.*;
import java.util.*;
public class Test extends ParentTest {
	public Test(){

	}
    public void testf(){
	System.out.print(" fucntion of Test");
    }
    public static void main(String args[]){
	ParentTest test = new Test();
	test.testf();
    }
}
