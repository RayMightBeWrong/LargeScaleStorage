public class DataServerMessage {
    private boolean inner;
    private String nodeID;
    private String clientID;
    private String type;
    private String message;

    public DataServerMessage(){}

    public DataServerMessage(boolean inner, String nodeID, String clientID, String type, String message){
        this.inner = inner;
        this.nodeID = nodeID;
        this.clientID = clientID;
        this.type = type;
        this.message = message;
    }

    public DataServerMessage(String[] command){
        if (command.length == 5){
            if (command[0].equals("INNER")){
                this.inner = true;
            }
            else if (command[0].equals("OUTER"))
                this.inner = false;

            this.nodeID = command[1];
            this.clientID = command[2];
            this.type = command[3];
            this.message = command[4];
        }
        else{
            this.message = null;
        }
    }

    //public Map<Integer, Integer> getClock(){ return this.clock; }
    public boolean getInner(){ return this.inner; }
    public String getNodeID(){ return this.nodeID; }
    public String getClientID(){ return this.clientID; }
    public String getType(){ return this.type; }
    public String getMessage(){ return this.message; }

    public void setInner(boolean inner){ this.inner = inner; }
    public void setNodeID(String nodeID){ this.nodeID = nodeID; }
    public void setClientID(String clientID){ this.clientID = clientID; }
    public void setType(String type){ this.type = type; }
    public void setMessage(String msg){ this.message = msg; }


    public String constructMessage(){
        StringBuilder sb;
        if (inner)
            sb = new StringBuilder("INNER!");
        else
            sb = new StringBuilder("OUTER!");

        sb.append(nodeID + "!");
        sb.append(clientID + "!");
        sb.append(type + "!");
        sb.append(message);
        
        return sb.toString();
    }
}