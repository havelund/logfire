package examples.java;

import rete.*;

class MyMonitor extends Monitor {}

public class Main {
	public static void main(String[] args) {
		System.out.println("running main");
		Monitor m = new MyMonitor();
		m.printRules();
	}
}
