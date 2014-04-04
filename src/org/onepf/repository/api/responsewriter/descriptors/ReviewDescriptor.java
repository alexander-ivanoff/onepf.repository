package org.onepf.repository.api.responsewriter.descriptors;

        import org.onepf.repository.api.responsewriter.ResponseWriter;
        import org.onepf.repository.api.responsewriter.Writable;
        import org.onepf.repository.api.responsewriter.WriteException;


/**
 * Created by ivanoff on 31.03.14.
 */
public class ReviewDescriptor extends AbstractDescriptor implements Writable {
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