package ids.unicam.models;


import ids.unicam.models.attori.Contributor;

import java.util.List;

public interface Observer {
    List<Notifica> riceviNotifiche(Contributor contributor);


}
