package ids.unicam.models.Service;

import ids.unicam.models.Repository.TuristaAutenticatoRepository;
import ids.unicam.models.attori.GestoreController;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Calendar;
import java.util.GregorianCalendar;

@Service
public class TuristaAutenticatoService{

    private final TuristaAutenticatoRepository turistaAutenticatoRepository;

  //  private final GestorePiattaforma gestorePiattaforma;
    @Autowired
    public TuristaAutenticatoService(TuristaAutenticatoRepository turistaAutenticatoRepository) {
        this.turistaAutenticatoRepository = turistaAutenticatoRepository;
      //  this.gestorePiattaforma = gestorePiattaforma;
    }


    public void eseguiOperazioniCrud(){
       // gestorePiattaforma.getGestoreController().registraTurista("Leonardo", "Compagnucci", new GregorianCalendar(1998, Calendar.JANUARY,1),"user","pass");

    }
    public TuristaAutenticato salvaTurista(TuristaAutenticato turistaAutenticato){
        turistaAutenticato= turistaAutenticatoRepository.save(turistaAutenticato);
        return turistaAutenticato;

    }



}
