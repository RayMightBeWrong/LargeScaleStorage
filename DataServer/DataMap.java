import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
            else
                res = null;

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

    public String moveRequest(String myID, String otherID) throws Exception{
        StringBuilder res = new StringBuilder();

        List<Integer> toMove = keysToMove(myID, otherID);
        int i = 0;
        for (Integer hash: toMove){
            Map<Integer, String> keyData = this.data.get(hash);
            int j = 0;
            for (Map.Entry<Integer, String> entry: keyData.entrySet()){
                int version = entry.getKey();
                String value = entry.getValue();
                Map<Integer, Map<String, Integer>> keyDeps = this.deps.get(hash);
                Map<String, Integer> versionDeps = keyDeps.get(version);
                res.append(String.valueOf(hash) + ";");
                res.append(String.valueOf(version) + ";");
                res.append(value + ";");
                res.append(buildDeps(versionDeps));

                j++;
                if (j == keyData.size() && i + 1 == toMove.size())
                    ;
                else
                    res.append("@");
            }

            i++;
        }

        return res.toString();
    }

    public void addElem(int hash, int version, String value, Map<String, Integer> new_deps){
        try{
            this.lock.lock();

            if (this.data.containsKey(hash)){
                Map<Integer, String> values = this.data.get(hash);
                values.put(version, value);
                this.data.put(hash, values);

                int currentRecent = this.mostRecent.get(hash);
                if (version > currentRecent){
                    this.mostRecent.put(hash, version);
                }

                Map<Integer, Map<String, Integer>> currentDeps = this.deps.get(hash);
                currentDeps.put(version, new_deps);
                this.deps.put(hash, currentDeps);
            }
            else{
                Map<Integer, String> values = new HashMap<>();
                values.put(version, value);
                this.data.put(hash, values);

                this.mostRecent.put(hash, version);

                Map<Integer, Map<String, Integer>> currentDeps = new HashMap<>();
                currentDeps.put(version, new_deps);
                this.deps.put(hash, currentDeps);
            }
        }
        finally{
            this.lock.unlock();
        }
    }

    public List<Integer> keysToMove(String myID, String otherID) throws Exception{
        List<Integer> res = new ArrayList<>();
        Map<String, Integer> hashs = new HashMap<>();
        hashs.put(myID, 1);
        hashs.put(otherID, 2);

        for (Map.Entry<Integer, Integer> entry: this.mostRecent.entrySet()){
            int hash = entry.getKey();

            String closest = getClosest(hash, hashs);
            if (closest.equals(otherID))
                res.add(hash);
        }

        return res;
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

    private String getClosest(int hashcode, Map<String, Integer> hashs) throws Exception {
        String res = "";
        BigInteger res_diff = BigInteger.valueOf(Integer.MAX_VALUE);
        res_diff = res_diff.add(res_diff);

        // if the key's hashcode is smaller than all of the servers' hashcodes
        boolean less = true;

        for (Map.Entry<String, Integer> entry: hashs.entrySet()){
            int nodeHash = Integer.parseInt(entry.getKey());
            // hashcode - nodeHash
            BigInteger diff = BigInteger.valueOf(hashcode).subtract(BigInteger.valueOf(nodeHash));
            
            if (less && diff.compareTo(BigInteger.valueOf(0)) > 0){
                less = false;
                res = entry.getKey();
                res_diff = diff;
            }
            // if less is true => res_diff < 0
            // if diff < res_diff then it is closer to the key's hashcode in the chord 
            else if (less && diff.compareTo(res_diff) < 0){
                res = entry.getKey();
                res_diff = diff;
            }
            else if (less == false && diff.compareTo(BigInteger.valueOf(0)) >= 0 && diff.compareTo(res_diff) < 0){
                res = entry.getKey();
                res_diff = diff;
            }
        }

        return res;
    }
}