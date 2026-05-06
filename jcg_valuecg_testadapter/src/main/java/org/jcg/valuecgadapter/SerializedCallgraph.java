package org.jcg.valuecgadapter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.zip.GZIPInputStream;

import com.google.gson.Gson;

public class SerializedCallgraph {
	public List<Edge> edges = new ArrayList<>();
	public Set<Edge> edgesSet = new HashSet<>();

	public static SerializedCallgraph readFromFile(File file) throws IOException {
		Gson gson = new Gson();
		try (FileReader fr = new FileReader(file)) {
			return readIn(gson, fr);
		}
	}
	public static SerializedCallgraph readFromFileCompressed(File file) throws IOException {
		Gson gson = new Gson();
		try (InputStreamReader fr = new InputStreamReader( new GZIPInputStream(new FileInputStream(file))) ){
			return readIn(gson, fr);
		}
	}


	private static SerializedCallgraph readIn(Gson gson, InputStreamReader fr) {
		SerializedCallgraph s = gson.fromJson(fr, SerializedCallgraph.class);
		for (Edge e : s.edges) {
			e.normalize();
		}
		s.edgesSet = new HashSet<>(s.edges);
		return s;
	}

	public void removeClinits() {
		Iterator<Edge> it = edges.iterator();
		while (it.hasNext()) {
			Edge e = it.next();
			if (e.kind.equals("CLINIT") || e.targetMethod.endsWith("<clinit>()>"))
			{
				it.remove();
			}
		}
	}

	public void removeAndroidSystem() {
		Iterator<Edge> it = edges.iterator();
		while (it.hasNext()) {
			Edge e = it.next();
			if (e.targetMethod.startsWith("<android.support"))
				continue;
			if (e.targetMethod.startsWith("<android.") || e.targetMethod.startsWith("<java.") || e.targetMethod.startsWith("<javax."))
				it.remove();
 			else if (e.sourceMethod.startsWith("<dummyMainClass: "))
				it.remove();

		}
	}

	public static SerializedCallgraph computeIntersection(SerializedCallgraph c1, SerializedCallgraph c2) {
		SerializedCallgraph res = new SerializedCallgraph();
		
/*		if (c1.edges.size() > c2.edges.size()) {
			SerializedCallgraph swap = c2;
			c2 = c1;
			c1 = swap;
		}*/
		for (Edge e : c1.edges) {
			if (c2.edgesSet.contains(e)) {
				res.edges.add(e);
				res.edgesSet.add(e);
			} else
				;//System.out.println(e);
		}
		return res;
	}

	public Set<String> getMethods() {
		Set<String> methods = new HashSet<>();
		for (Edge m : edgesSet) {
			methods.add(m.sourceMethod);
			methods.add(m.targetMethod);
		}
		return methods;
	}
}
