import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class DataMap {
    private Map<Integer, String> data;
    private ReentrantLock lock;

    public DataMap(){
        this.data = new HashMap<>();
        this.lock = new ReentrantLock();
    }

    public String readOne(String key) throws Exception{
        try{
            String res = "";
            // TODO: parar de usar locks
            this.lock.lock();
            int hash = key.hashCode();
            if (this.data.containsKey(hash))
                res = this.data.get(hash);

            return res;
        }
        finally{
            this.lock.unlock();
        }
    }

    public List<String> read(List<String> args) throws Exception{
        try{
            List<String> res = new ArrayList<>();
            // TODO: parar de usar locks
            this.lock.lock();

            for (int i = 0; i < args.size(); i++){
                String key = args.get(i);
                int hash = key.hashCode();
                if (!this.data.containsKey(hash))
                    res.add(null);
                else
                    res.add(this.data.get(hash));
            }

            return res;
        }
        finally{
            this.lock.unlock();
        }
    }

    public void write(String key, String value) throws Exception{
        try{
            // TODO: parar de usar locks
            this.lock.lock();
            int hash = key.hashCode();
            this.data.put(hash, value);
        }
        finally{
            this.lock.unlock();
        }
    }
}