var Studio = function(){
    var self = this;
    
    self.log = function(){
        if(console) console.log.apply(null, arguments);
    }

    self.idCounter = 0;
    self.toUpload = {};
    self.toDelete = [];
    
    var initUpload = function(root){
        self.log("Init upload", root);
        // FIXME: Implement logic to undo a removal
        var registerRemove = function(remove){
            remove.addEventListener("click", function(){
                var root = remove.parentNode;
                if(root.dataset.tid){
                    delete self.toUpload[root.dataset.tid];
                } else {
                    self.toDelete.push(root.dataset.id);
                }
                root.parentNode.removeChild(root);
            });
        };
        
        var registerNewImage = null;
        var showFile = function(root, file){
            var image = document.createElement("img");
            var remove = document.createElement("label");
            if(file){
                var reader = new FileReader();
                root.dataset.tid = self.idCounter++;
                self.toUpload[root.dataset.tid] = file;
                reader.onload = function(){ image.src = reader.result; };
                reader.readAsDataURL(file);
            }
            // FIXME: D&D
            remove.classList.add("remove");
            remove.innerHTML = '<i class="fas fa-fw fa-trash-alt"></i>';
            registerRemove(remove);
            root.appendChild(image);
            root.appendChild(remove);
        };
        
        var addNewImage = function(old){
            self.log("New image", old);
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

        [].forEach.call(root.querySelectorAll(".image .remove"), registerRemove);

        root.querySelector("[type=submit]").addEventListener("click", function(ev){
            var form = new FormData();
            form.append("upload", root.querySelector("[name=upload]").value);
            form.append("title", root.querySelector("[name=title]").value);
            form.append("description", root.querySelector("[name=description]").value);
            form.append("tags", root.querySelector("[name=tags]").value);
            form.append("data-format", "json");
            for(var i in self.toUpload){
                form.append("file[]", self.toUpload[i]);
            }
            for(var i in self.toDelete){
                form.append("delete[]", self.toDelete[i]);
            }
            var request = new XMLHttpRequest();
            request.responseType = 'json';
            request.onload = function(ev){
                self.log("Submission complete", request);
                if(request.status == 200){
                    window.location = request.response["data"]["url"];
                }else{
                    document.querySelector("#error").innerHTML = request.response["message"];
                }
            };
            request.open("POST", ev.target.getAttribute("formaction"));
            self.log("Submitting", form);
            request.send(form);
            ev.preventDefault();
            return false;
        });
    };

    var initGallery = function(root){
        self.log("Init gallery", root);
    };

    var initView = function(root){
        self.log("Init view", root);
    };

    self.init = function(root){
        var upload = root.querySelector(".upload");
        var gallery = root.querySelector(".gallery");
        var view = root.querySelector(".view");
        
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
    };
};

var studio = null;
document.addEventListener("DOMContentLoaded", function(){
    studio = new Studio();
    studio.init(document);
});
