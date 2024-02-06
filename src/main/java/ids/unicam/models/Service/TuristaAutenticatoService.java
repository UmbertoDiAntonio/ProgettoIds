package ids.unicam.models.Service;

import ids.unicam.models.Repository.TuristaAutenticatoRepository;
import ids.unicam.models.attori.GestoreController;
import ids.unicam.models.attori.GestorePiattaforma;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Calendar;
import java.util.GregorianCalendar;

@Service
public class TuristaAutenticatoService{

    private TuristaAutenticatoRepository turistaAutenticatoRepository;

    private GestorePiattaforma gestorePiattaforma;
    @Autowired
    public TuristaAutenticatoService(TuristaAutenticatoRepository turistaAutenticatoRepository, GestorePiattaforma gestorePiattaforma) {
        this.turistaAutenticatoRepository = turistaAutenticatoRepository;
        this.gestorePiattaforma = gestorePiattaforma;
    }


    public void eseguiOperazioniCrud(){
        gestorePiattaforma.getGestoreController().registraTurista("Leonardo", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1),"user","pass");
        turistaAutenticatoRepository.save(gestorePiattaforma.getGestoreController().getUtentiController().getTuristi().getLast());
    }



}
