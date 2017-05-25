package examples.crv15.acsvreader.java;

import java.io.*;
import java.util.*;
import org.apache.commons.csv.*;

class Monitor {
	String monitorName;

	public static Boolean PRINT = false;

	public Monitor(String monitorName) {
		this.monitorName = monitorName;
		System.out.println("BEGIN");
	}

	void addMapEvent(Map<String, String> map) {
		if (PRINT) {
			for (String key : map.keySet()) {
				System.out.print(key + "=" + map.get(key) + " ");
			}
			System.out.println();
		}
	}
	
	void terminate() {
		System.out.println("END");
	}
}

class CSVMonitor extends Monitor {
	public CSVMonitor(String monitorName) {
		super(monitorName);
	}

	private String symbolOf(int index) {
		String result = "missed";
		switch (index) {
		case 1:
			result = "one";
			break;
		case 2:
			result = "two";
			break;
		case 3:
			result = "three";
			break;
		case 4:
			result = "four";
			break;
		case 5:
			result = "five";
			break;
		case 6:
			result = "six";
			break;
		case 7:
			result = "seven";
			break;
		case 8:
			result = "eight";
			break;
		case 9:
			result = "nine";
			break;
		}
		return result;
	}

	public void submit(CSVRecord record) {
		Map<String, String> map = new HashMap();
		map.put("kind", record.get(0));
		for (int i = 1; i < record.size(); i++) {
			map.put(symbolOf(i), record.get(i));
		}
		addMapEvent(map);
	}
}

public class Main {
	public static void main(String[] args) throws IOException {
		CSVMonitor monitor = new CSVMonitor(args[0]);
		Reader in = new BufferedReader(new FileReader(args[1]));
		Iterable<CSVRecord> records = CSVFormat.DEFAULT.withHeader().parse(in);
		for (CSVRecord record : records) {
			monitor.submit(record);
		}
		monitor.terminate();
		in.close();
	}
}
