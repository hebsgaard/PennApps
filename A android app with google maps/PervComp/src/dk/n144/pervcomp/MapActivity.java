package dk.n144.pervcomp;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.widget.TextView;

import com.google.android.gms.maps.CameraUpdate;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.MapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;

public class MapActivity extends Activity implements LocationListener {
	
	static TextView header;
	static String user_ID;

	static private LocationManager lManager;
	static private GoogleMap mMap;
	static Marker userMarker = null;
	static boolean cameraSet = false;
	final static float zoomLevel = 10.0f;
	
	//final static String serverIP = "10.0.0.2";
	//final static String serverIP = "192.168.123.19";
	final static String serverIP = "84.238.87.199";
	final static String serverAddress = "http://" + serverIP + ":8080";
	final static String getLocationListPath = "/getLocationList";
	final static String setLocationPath = "/setLocation";
	final static String stringSeparator = " "; //Must fit the stringSeparator on the server
	

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_map);
		
		Intent intent = getIntent();
		user_ID = intent.getStringExtra("user_name");
		header = (TextView) findViewById(R.id.header);
		
		// Når brugerens lokation opdateres, skal denne klasse opdateres:
		lManager = (LocationManager) getSystemService(LOCATION_SERVICE);
		lManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 1000L, 10.0f, this);
		lManager.requestLocationUpdates(LocationManager.NETWORK_PROVIDER, 1000L, 10.0f, this);
		mMap = ((MapFragment) getFragmentManager().findFragmentById(R.id.map)).getMap();

		// Dette kald sørger for at komme markers for forskellige brugere på kortet
		(new SetIDLocations()).execute(serverAddress + getLocationListPath);
    }


	
	
	@Override
	public void onLocationChanged(Location location) { // Det er detekteret en ny lokation
		LatLng ll = new LatLng(location.getLatitude(), location.getLongitude());
		
		// Tilføj 
		if (userMarker == null) {
			userMarker = mMap.addMarker(new MarkerOptions()
	       		.position(ll)
	       		.title(user_ID)
	       		.icon(BitmapDescriptorFactory.fromAsset("down_arrow.png")));
		} else {
			userMarker.setPosition(ll);
		}

		if (!cameraSet) {// Set the camera the first time
			CameraUpdate cu = CameraUpdateFactory.newLatLngZoom(ll, zoomLevel);
			mMap.moveCamera(cu);
			cameraSet = true;
		}
		
		(new UpdateUserLocation(ll)).execute(serverAddress + setLocationPath);
	}
	
	@Override
	public void onProviderDisabled(String provider) {
		header.setText("Provider disabed: " + provider);
	}

	@Override
	public void onProviderEnabled(String provider) {
		header.setText("Provider enabled: " + provider);
	}

	@Override
	public void onStatusChanged(String provider, int status, Bundle extras) {
		header.setText("Status changed for provider: " + provider);
	}
	
	private class SetIDLocations extends AsyncTask<String, Void, List<MarkerOptions> > {
		MarkerOptions userMo = null;
		
		@Override
		protected List<MarkerOptions> doInBackground(String... params) {
			List<MarkerOptions> result = new ArrayList<MarkerOptions>();
			try{
				URL url = new URL(params[0]);
				HttpURLConnection connection = (HttpURLConnection) url.openConnection();
				
				connection.setRequestMethod("GET");
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
				String line = "";
				while((line = reader.readLine()) != null){

					String[] lineSplit = line.split(stringSeparator);
					double latitude = Double.parseDouble(lineSplit[1]);
					double longitude = Double.parseDouble(lineSplit[2]);
					LatLng ll = new LatLng(latitude, longitude);
					MarkerOptions mo = new MarkerOptions().position(ll).title(lineSplit[0]);

					// Change color if this marker is for the user
					if (user_ID.equals(lineSplit[0])) {
						userMo = mo.icon(BitmapDescriptorFactory.defaultMarker(BitmapDescriptorFactory.HUE_AZURE));
						continue;
					}
					result.add(mo);
				}
			} catch (Exception e){				
				Log.d("MainActivity", "Exception:", e);
			}
			return result;
		}
		
	     protected void onPostExecute(List<MarkerOptions> markerOptionsList) {
	    	 for (MarkerOptions mo : markerOptionsList) {
	    		 mMap.addMarker(mo);
	    	 }
	    	 //Hvis userMarker er null, betyder det at MapActivity.OnLocationChanged endnu ikke er blevet kaldt.
	    	 //I så fald benytter vi serverens position for brugeren (kun hvis den findes, ie hvis userMo != null)
	    	 if (userMarker == null && userMo != null) {
	    		 userMarker = mMap.addMarker(userMo);
	    	 }
	     }
	}
	
	
	private class UpdateUserLocation extends AsyncTask<String,Void,String> {
		LatLng ll;
		UpdateUserLocation(LatLng latlng) {
			ll = latlng;
		}
		@Override
		protected String doInBackground(String... params) {
			String response = "";
			try{
				URL url = new URL(params[0]);
				HttpURLConnection connection = (HttpURLConnection) url.openConnection();
				
				String msg = user_ID + "\n" + ll.latitude + "\n" + ll.longitude + "\n";

				connection.setRequestMethod("POST");
				OutputStream o = connection.getOutputStream();
				BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(o));
				bw.write(msg);
				bw.flush();
				bw.close();
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
				String line = "";
				while((line = reader.readLine()) != null){
					response += line + "\n";
				}
			} catch (Exception e){				
				Log.d("MainActivity", "Exception:", e);
			}
			return response;
		}
		
		protected void onPostExecute(String result) {
			header.setText(result);
	    }
	}
}
