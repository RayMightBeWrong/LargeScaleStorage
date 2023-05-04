import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class DataMap {
    private Map<String, String> data;
    private ReentrantLock lock;

    public DataMap(){
        this.data = new HashMap<>();
        this.lock = new ReentrantLock();
    }

    public List<String> read(List<String> args){
        try{
            List<String> res = new ArrayList<>();
            this.lock.lock();

            for (int i = 0; i < args.size(); i++){
                String key = args.get(i);
                if (!this.data.containsKey(key))
                    res.add(null);
                else
                    res.add(this.data.get(key));
            }

            return res;
        }
        finally{
            this.lock.unlock();
        }
    }

    public void write(String key, String value){
        try{
            this.lock.lock();
            this.data.put(key, value);
        }
        finally{
            this.lock.unlock();
        }
    }
}