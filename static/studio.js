document.addEventListener("DOMContentLoaded", function(){
    var log = function(){
        if(console) console.log.apply(null, arguments);
    }
    
    var initUpload = function(root){
        log("Init upload", root);

        var idCounter = 0;
        var toUpload = {};

        var registerNewImage = null;
        var showFile = function(root, file){
            var reader = new FileReader();
            var image = document.createElement("img");
            var remove = document.createElement("i");
            var id = idCounter++;
            toUpload[id] = file;
            reader.onload = function(){ image.src = reader.result; };
            reader.readAsDataURL(file);
            // FIXME: D&D
            remove.classList.add("remove", "fas", "fa-fw", "fa-trash-alt");
            remove.addEventListener("click", function(){
                delete toUpload[id];
                root.removeChild(image);
                root.removeChild(remove);
            });
            root.appendChild(image);
            root.appendChild(remove);
        };
        
        var addNewImage = function(old){
            log("New image", old);
            // Do this first so subsequent files are not required.
            var file = old.querySelector("[type=file]");
            file.removeAttribute("required");
            // Clone and register a copy of the selector.
            var newImage = old.cloneNode(true);
            newImage.querySelector("[type=file]").value = "";
            root.querySelector(".images").appendChild(newImage);
            registerNewImage(newImage);
            // Turn the old selector into an image preview.
            file.removeAttribute("id");
            old.classList.remove("new-image");
            old.classList.add("image");
            var label = old.querySelector("label");
            label.parentNode.removeChild(label);
            // Load the image files.
            for(var i=0; i<file.files.length; i++){
                showFile(old, file.files[i]);
            }
        };
        
        registerNewImage = function(old){
            old.querySelector("[type=file]").addEventListener("change", function(){
                addNewImage(old);
            });
        };
        
        registerNewImage(root.querySelector(".new-image"));

        root.querySelector("[type=submit]").addEventListener("click", function(ev){
            ev.preventDefault();
            // FIXME: Submit
            return false;
        });
    };

    var initGallery = function(root){
        log("Init gallery", root);
    };

    var initView = function(root){
        log("Init view", root);
    };

    var init = function(root){
        var upload = root.querySelector(".upload");
        var gallery = root.querySelector(".gallery");
        var view = root.querySelector(".view");
        
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
    };

    init(document);
});
