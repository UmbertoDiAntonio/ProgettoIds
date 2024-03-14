package ids.unicam.models;


import ids.unicam.models.contenuti.notifiche.Notifica;

import java.util.List;

public interface Observer {
    List<Notifica> visualizzaNotifiche(String usernameTurista) throws IllegalArgumentException;

}
