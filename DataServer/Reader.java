import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Reader {
    private Map<Integer, Integer> missing;
    private Map<Integer, Map<String, String>> kvs;
    private Map<Integer, PrintWriter> outputs;

    public Reader(){
        this.missing = new HashMap<>();
        this.kvs = new HashMap<>();
        this.outputs = new HashMap<>();
    }

    public void createNewReadInstance(int clock, List<String> args, PrintWriter out){
        this.missing.put(clock, args.size());

        Map<String, String> kv_instance = new HashMap<>();
        for (int i = 0; i < args.size(); i++)
            kv_instance.put(args.get(i), "");
        
        this.kvs.put(clock, kv_instance);
        this.outputs.put(clock, out);
    }

    public boolean handleReadOk(int clock, String key, String value){
        Map<String, String> kv_instance = kvs.get(clock);

        int missin_ = -1;
        if (kv_instance.containsKey(key)){
            kv_instance.put(key, value);
            kvs.put(clock, kv_instance);

            missin_ = missing.get(clock) - 1;
            this.missing.put(clock, missin_);
        }

        return missin_ == 0; 
    }

    public Map<String, String> getKVInstance(int clock){
        return this.kvs.get(clock);
    }

    public PrintWriter getOutput(int clock){
        return this.outputs.get(clock);
    }
}
