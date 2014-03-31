package org.onepf.repository.utils.responsewriter.descriptors;

        import org.onepf.repository.utils.responsewriter.ResponseWriter;
        import org.onepf.repository.utils.responsewriter.Writable;
        import org.onepf.repository.utils.responsewriter.WriteException;


/**
 * Created by ivanoff on 31.03.14.
 */
public class ReviewDescriptor implements Writable {
    public String packageName;
    public String version;
    public int build;
    public String lastUpdate;
    public String deviceModel;
    public String deviceName;
    public String country;
    public int stars;
    public String userName;
    public String userUrl;
    public String title;
    public String body;
    public int currPageHash;
    public int prevPageHash;

    @Override
    public void write(ResponseWriter responseWriter) throws WriteException {
        responseWriter.write(this);
    }
}