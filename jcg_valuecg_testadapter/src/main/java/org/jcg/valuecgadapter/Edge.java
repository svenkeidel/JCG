package org.jcg.valuecgadapter;

import java.util.Objects;

public class Edge {
	public String sourceMethod;
	public String targetMethod;
	public String sourceStatement;
	public String sourceStatementNormalized;
	public String kind;
	public Integer lineNumber;
	public String sourceICCStatement;
	public String destICCClass;
	public String[] declaringClasses;
	public String reasoning;

	public void normalize() {

		String originalUnit = sourceStatement;
		String rep = originalUnit.replaceAll("\\$[a-zA-Z0-9_-]*", "\\$v");
		rep = rep.replace("this", "$v");
		if (rep.startsWith("virtualinvoke ") || rep.startsWith("interfaceinvoke ")) {
			rep = rep.substring(0, rep.indexOf("<") + 1) + rep.substring(rep.indexOf(":") + 1);
		}
		if (rep.equals(originalUnit) && !rep.equals("nop") && !originalUnit.startsWith("<")) {
			int inv = rep.indexOf("invoke ");
			String base = rep.substring(inv + 7);
			if (!base.startsWith("<") && base.contains(".")) {
				base = base.substring(0, base.indexOf("."));
				rep = rep.replace(base, "$v");
				int s = rep.indexOf("(", rep.indexOf("(") + 1) + 1;
				int next2 = rep.indexOf(")", s);
				if (next2 != -1) {
					String params = rep.substring(s, next2);
					for (String param : params.split(",")) {
						param = param.trim();
						rep = rep.replace(param, "$v");
					}
				}
			}
		}
		while (rep.contains("$v$v"))
			rep = rep.replace("$v$v", "$v");
		this.sourceStatementNormalized = rep;

	}

	@Override
	public int hashCode() {
		return Objects.hash(sourceMethod, targetMethod);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Edge other = (Edge) obj;
		return Objects.equals(sourceMethod, other.sourceMethod)
				&& Objects.equals(sourceStatementNormalized, other.sourceStatementNormalized)
				&& Objects.equals(targetMethod, other.targetMethod);
	}

	@Override
	public String toString() {
		return String.format("%s: %s -> %s (%s)", kind, sourceMethod, targetMethod, sourceStatement);
	}
}
