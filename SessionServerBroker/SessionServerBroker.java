package zeromq.my_tests;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class SessionServerBroker {
    public static void main(String[] args) {
        try(ZContext context = new ZContext();
            ZMQ.Socket router = context.createSocket(SocketType.ROUTER);
            ZMQ.Socket pub = context.createSocket(SocketType.PUB)){

            router.bind("tcp://*:" + args[0]);
            pub.bind("tcp://*:" + args[1]);

            while(true){
                byte[] m = router.recv();
                System.out.println(new String(m));
                if (router.hasReceiveMore()) {
                    pub.sendMore(m);
                } else {
                    pub.send(m);
                }
            }
        }
    }
}
