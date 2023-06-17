import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class DataMap {
    private Map<Integer, Map<Integer, String>> data;    // <key_hash, <version, value>>
    private Map<Integer, Map<Integer, Map<String, Integer>>> deps;  // <key_hash, <version, <dependency_key, dependency_version>>>
    private Map<Integer, Integer> mostRecent;
    private ReentrantLock lock;

    public DataMap(){
        this.data = new HashMap<>();
        this.deps = new HashMap<>();
        this.mostRecent = new HashMap<>();
        this.lock = new ReentrantLock();
    }

    public Map<String, String> readOne(String key) throws Exception{
        try{
            Map<String, String> res = new HashMap<>();
            this.lock.lock();
            int hash = key.hashCode();

            if (this.data.containsKey(hash)){
                int last_version = this.mostRecent.get(hash);
                Map<Integer, String> values = this.data.get(hash);
                res.put("value", values.get(last_version));
                res.put("version", String.valueOf(last_version));
                res.put("deps", buildDeps(this.deps.get(hash).get(last_version)));
            }

            return res;
        }
        finally{
            this.lock.unlock();
        }
    }

    public String readVersion(String key, String version) throws Exception{
        try{
            String res = "";
            this.lock.lock();
            int hash = key.hashCode();

            if (this.data.containsKey(hash)){
                Map<Integer, String> values = this.data.get(hash);
                res = values.get(Integer.parseInt(version));
            }

            return res;
        }
        finally{
            this.lock.unlock();
        }
    }

    public int write(String key, String value, Map<String, Integer> new_deps) throws Exception{
        try{
            this.lock.lock();
            int new_version = 1;
            int hash = key.hashCode();

            if (this.data.containsKey(hash)){
                new_version = this.mostRecent.get(hash) + 1;
                Map<Integer, String> values = this.data.get(hash);
                values.put(new_version, value);
                this.data.put(hash, values);
                this.mostRecent.put(hash, new_version);
                Map<Integer, Map<String, Integer>> dependencies = this.deps.get(hash);
                dependencies.put(new_version, new_deps);
                this.deps.put(hash, dependencies);
            }
            else{
                Map<Integer, String> values = new HashMap<>();
                values.put(new_version, value);
                this.data.put(hash, values);
                this.mostRecent.put(hash, new_version);
                Map<Integer, Map<String, Integer>> dependencies = new HashMap<>();
                dependencies.put(new_version, new_deps);
                this.deps.put(hash, dependencies);
            }

            return new_version;
        }
        finally{
            this.lock.unlock();
        }
    }

    private String buildDeps(Map<String, Integer> dependencies){
        StringBuilder sb = new StringBuilder("[");
    
        int i = 0;
        for (Map.Entry<String, Integer> entry: dependencies.entrySet()){
            sb.append("{" + entry.getKey() + "," + Integer.valueOf(entry.getValue()) + "}");
            i ++;
            if (i < dependencies.size()){
                sb.append("/");
            }
        }
        sb.append("]");

        return sb.toString();
    }
}