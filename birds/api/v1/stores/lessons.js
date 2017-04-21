module.exports.uploadLessonData = async (lesson, data) => {
    return new Promise((resolve, reject) => {
        let params = {
            Key: `lessons/${lesson.id}`,
            Body: data
        };
        s3bucket.upload(params, err => {
            if (err) reject(err);
            else resolve();
        });
    });
};

module.exports.getLessonData = async lesson => {
    return new Promise((resolve, reject) => {
        let params = {
            Key: `lessons/${lesson.id}`
        };
        s3bucket.getObject(params, function(err, data) {
            if (err) reject(err);
            else resolve(data.Body.toString());
        });
    });
};
