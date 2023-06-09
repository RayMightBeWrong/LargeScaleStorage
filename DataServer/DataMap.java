import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class DataMap {
    private Map<Integer, Map<Integer, String>> data;
    private Map<Integer, Integer> mostRecent;
    private ReentrantLock lock;

    public DataMap(){
        this.data = new HashMap<>();
        this.mostRecent = new HashMap<>();
        this.lock = new ReentrantLock();
    }

    public String readOne(String key) throws Exception{
        try{
            String res = "";
            this.lock.lock();
            int hash = key.hashCode();

            if (this.data.containsKey(hash)){
                int last_version = this.mostRecent.get(hash);
                Map<Integer, String> values = this.data.get(hash);
                res = values.get(last_version);
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

    public int write(String key, String value) throws Exception{
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
            }
            else{
                Map<Integer, String> values = new HashMap<>();
                values.put(new_version, value);
                this.data.put(hash, values);
                this.mostRecent.put(hash, new_version);
            }
            /* 
            for (Map.Entry<Integer, Map<Integer, String>> entry: this.data.entrySet()){
                System.out.println("=== HASH KEY: " + entry.getKey() + " ===");
                System.out.println("most recent: " + this.mostRecent.get(entry.getKey()));
                for (Map.Entry<Integer, String> entry2: entry.getValue().entrySet()){
                    System.out.println("version: " + entry2.getKey() + " | value: " + entry2.getValue());
                }    
            } */
            return new_version;
        }
        finally{
            this.lock.unlock();
        }
    }
}